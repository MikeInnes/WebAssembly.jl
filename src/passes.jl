applyblock(f, x::Union{Block,Loop}) = typeof(x)(f(x.body))
applyblock(f, x::If) = If(f(x.t), f(x.f))
applyblock(f, x) = x

walk(x, inner, outer) = outer(applyblock(xs -> map(inner, xs), x))

postwalk(f, x) = walk(x, x -> postwalk(f, x), f)
prewalk(f, x)  = walk(f(x), x -> prewalk(f, x), identity)

# remove nops

nops(x) = prewalk(x -> applyblock(is -> filter(i -> i ≠ nop, is), x), x)

# dead code

function deadcode(x)
  prewalk(x) do x
    applyblock(x) do is
      i = findfirst(b -> b isa Branch && !b.cond, is)
      i == nothing ? is : is[1:i]
    end
  end
end

mapbranches(f, b, level = -1) = applyblock(is -> mapbranches.(f, is, level+1), b)
mapbranches(f, b::Branch, level = -1) = f(b, level)

function branches_to(b)
  result = false
  mapbranches(b) do b, l
    b.level==l && (result = true)
    b
  end
  return result
end

# After removing a block that has no branches to it, decrement all of the
# branches that point to any level above it.
function decbranches(i)
  mapbranches(i) do b, l
    b.level > l ? Branch(b.cond, b.level-1) : b
  end
end

# After adding a block increment all branches pointing to a level above it.
function incbranches(i)
  mapbranches(i) do b, l
    b.level >= l ? Branch(b.cond, b.level+1) : b
  end
end

# recover ifs

function makeifs(code)
  prewalk(code) do x
    x isa Block || return x
    i = findfirst(x -> x isa Branch, x.body)
    i != nothing && x.body[i].cond || return x
    cond = x.body[1:i-1]
    cond[end] == Op(i32, :eqz) ? pop!(cond) : push!(cond, Op(i32, :eqz))
    fixbranches = incbranches(Block(x.body[i+1:end])).body
    return Block([cond..., If(fixbranches, [])])
  end
end

# remove unused blocks
# TODO: collapse blocks with the same head

function rmblocks(code)
  prewalk(code) do x
    applyblock(x) do is
      is′ = []
      for i in is
        i isa Block && !branches_to(i) ?
          append!(is′, decbranches(i).body) :
          push!(is′, i)
      end
      is′
    end
  end
end

optimise(b) = b |> deadcode |> makeifs |> rmblocks

using LightGraphs

# rig is the Register Interference Graph. out_neighbors(value id, rig) will give
# the ids of the values that are alive at the same time as the given value id.

# lines takes a value id and returns references to all the get/sets in the code
# so they can be updated quickly later.

# alive is essentially a set of currently used registers, but with the id of the
# value stored.

function liveness(func::Func)
  rig = SimpleGraph()
  alive = Dict{Int, Int}()
  lines = Vector{Vector{Vector{Int}}}()
  skippedsets = Vector()
  alive = liveness_(func.body.body, rig, lines, alive, [], [], false, skippedsets)
  # @show alive
  @show skippedsets
  @show map(length, lines) |> sum
  return rig, alive, lines, skippedsets
end

function liveness_(code, rig::SimpleGraph=SimpleGraph(), lines::Vector{Vector{Vector{Int}}}=[], alive::Dict{Int, Int}=[], branches=[], branch=[], loop::Bool=false, skippedsets=Vector())
  for i in length(code):-1:1
    x = code[i]
    if x isa Local
      # @show x, alive
      id = get!(alive, x.id) do
        id = if loop && haskey(branches[end][1], x.id)
          # println("So this happens, $x")
          branches[end][1][x.id]
        else
          add_vertex!(rig)
          push!(lines, [])
          nv(rig)
        end
        for v in values(alive)
          add_edge!(rig, id, v)
        end
        return id
      end
      push!(lines[id], push!(deepcopy(branch), i))
      # @show code[i], i
    # @show alive
    elseif x isa SetLocal
      # @show x
      if haskey(alive, x.id)
        id = alive[x.id]
        delete!(alive, x.id)
        push!(lines[id], push!(deepcopy(branch), i))
        # @show code[i]
        # push!(lines, id => Ref(code, i))
      else
        # TODO: Add support for drop / backprop to remove need for Drop.
        push!(skippedsets, push!(deepcopy(branch), i))
      end
    elseif x isa Block || x isa Loop
      push!(branches, (copy(alive), x isa Loop))
      push!(branch, i)
      alive = liveness_(code[i].body, rig, lines, alive, branches, branch, loop, skippedsets)
      @show sum(map(length, lines))
      pop!(branch)
      pop!(branches)
    elseif x isa If
      a_t = copy(alive)
      a_f = copy(alive)
      push!(branches, (copy(alive), false))
      push!(branch, i, 1)
      a_t = liveness_(code[i].t, rig, lines, a_t, branches, branch, loop, skippedsets)
      pop!(branch)
      push!(branch, 2)
      a_f = liveness_(code[i].f, rig, lines, a_f, branches, branch, loop, skippedsets)
      pop!(branch)
      pop!(branches)
      alive = merge(a_t, a_f)
    elseif x isa Branch
      # At a branch need to revert the state of alive to what it was just after
      # the block. (I.e. just before in terms of calculation order.)


      br = branches[end-x.level]
      a_b = copy(br[1])
      if !br[2] # If branching to a loop.
        a_b = liveness_(code[1:i-1], rig, lines, a_b, branches, branch, loop, skippedsets)
      else
        s_sets = Vector();
        a_b = liveness_(code[1:i-1], rig, lines, a_b, branches, branch, true, s_sets)
        # liveness(s_sets, rig, lines, alive, branches)
        # @show (1, alive)
        # s_sets will be in reverse order as required.
        for s in s_sets
          set = get_instr(code, s[length(branches)+1:end])
          @show set
          if haskey(a_b, set.id)
            push!(lines[a_b[set.id]], s)
            @show set
          else
            push!(skippedsets, s)
          end
        end
      end
      if x.cond
        a_f = copy(alive)
        # It should be possible to calculate this instead of starting over.
        a_f = liveness_(code[1:i-1], deepcopy(rig), deepcopy(lines), a_f, branches, branch, loop, skippedsets)
        @show alive
        @show a_b, a_f
        merge!(a_b, a_f)
      end
      # TODO: Might want to give a warning about deadcode here on unconditional
      # branches that come after some code.
      alive = a_b
      break
    end
  end

  if loop
    return alive
  elseif !isempty(skippedsets)
    println("Warning: Setting locals but values aren't used (Can replace with Drop).")
    println((map(getindex, skippedsets), map(s -> s, skippedsets)))
  end
  return alive
end


get_instr(code, branch) = get_at_loc(code, deepcopy(branch), x->x)

set_instr(code, branch, y) = get_at_loc(code, deepcopy(branch), x->y)
modify_instr(f, code, branch) = get_at_loc(code, deepcopy(branch), f)

function get_at_loc(code::Vector{Instruction}, branch, f)
  b = shift!(branch)
  if isempty(branch)
    return code[b] = f(code[b])
  else
    return get_at_loc(code[b], branch, f)
  end
end
get_at_loc(code::Func, branch, f) = get_at_loc(code.body.body, branch, f)
get_at_loc(code::Union{Block, Loop}, branch, f) = get_at_loc(code.body, branch, f)
function get_at_loc(code::If, branch, f)
  b = shift!(branch)
  if b == 1
    return get_at_loc(code.t, branch, f)
  elseif b == 2
    return get_at_loc(code.f, branch, f)
  end
  error()
end


function allocate_registers(func::Func)
  rig, alive, lines, skippedsets = func |> liveness
  coloring = rig |> greedy_color
  length(alive) == length(func.params) || error("Not all parameters are used.")

  c_to_r = Dict(coloring.colors[v] => r for (r, v) in alive)
  i = length(c_to_r) -1
  register_colours = [haskey(c_to_r, c) ? c_to_r[c] : i+=1 for c in 1:coloring.num_colors]

  @show register_colours
  @show coloring.colors

  col(value_id) = register_colours[coloring.colors[value_id]]
  for v in eachindex(lines)
    for l in lines[v]
      modify_instr(func, l) do s
        s isa Local ? Local(col(v)) : (s isa SetLocal ? SetLocal(s.tee, col(v)) : error())
        # s isa Local ? Local(v) : (s isa SetLocal ? SetLocal(s.tee, v) : error())
      end
    end
  end

  for s in skippedsets
    set_instr(func, s, Drop())
  end
  @show coloring
  @show func.params
  @show func.locals
  locals = func.locals[1:coloring.num_colors-length(func.params)]
  return Func(func.name, func.params, func.returns, locals, func.body)
end

using GraphLayout
function drawGraph(filename, graph)
  # Output the graph
  am = Matrix(adjacency_matrix(graph))
  loc_x, loc_y = layout_spring_adj(am)
  # draw_layout_adj(am, loc_x, loc_y, filename=filename, arrowlengthfrac=0)
  draw_layout_adj(am, loc_x, loc_y, filename=filename)
end
