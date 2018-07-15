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
  liveness_(func.body.body, rig)
  return rig
end

function liveness_(code, rig=SimpleGraph(), lines=Dict{Int, Vector{Ref}}(), alive=Dict{Int, Int}(), branches=[], loop::Bool=false, skippedsets=Vector())
  for i in length(code):-1:1
    x = code[i]
    if x isa Local
      # @show x, alive
      id = get!(alive, x.id) do
        id = if loop && haskey(branches[end][1], x.id)
          # println("So this happens, $x")
          branches[end][x.id]
        else
          add_vertex!(rig)
          nv(rig)
        end
        push!(lines, id => [])
        for v in values(alive)
          add_edge!(rig, id, v)
        end
        return id
      end
      push!(lines, id => push!(lines[id], Ref(code, i)))
    # @show alive
    elseif x isa SetLocal
      # @show x
      if haskey(alive, x.id)
        id = alive[x.id]
        delete!(alive, x.id)
        push!(lines, id => push!(lines[id], Ref(code, i)))
        # push!(lines, id => Ref(code, i))
      else
        # TODO: Add support for drop / backprop to remove need for Drop.
        push!(skippedsets, Ref(code, i))
      end
    elseif x isa Block || x isa Loop
      push!(branches, (copy(alive), x isa Loop))
      alive = liveness_(code[i].body, rig, lines, alive, branches, loop, skippedsets)
      pop!(branches)
    elseif x isa If
      a_t = copy(alive)
      a_f = copy(alive)
      push!(branches, (copy(alive), false))
      a_t = liveness_(code[i].t, rig, lines, a_t, branches, loop, skippedsets)
      a_f = liveness_(code[i].f, rig, lines, a_f, branches, loop, skippedsets)
      pop!(branches)
      alive = merge(a_t, a_f)
    elseif x isa Branch
      # At a branch need to revert the state of alive to what it was just after
      # the block. (I.e. just before in terms of calculation order.)


      br = branches[end-x.level]
      a_b = copy(br[1])
      if !br[2] # If branching to a loop.
        a_b = liveness_(code[1:i-1], rig, lines, a_b, branches, loop, skippedsets)
      else
        s_sets = Vector();
        a_b = liveness_(code[1:i-1], rig, lines, a_b, branches, true, s_sets)
        # liveness(s_sets, rig, lines, alive, branches)
        # @show (1, alive)
        # s_sets will be in reverse order as required.
        for s in s_sets
          set = getindex(s)
          if haskey(a_b, set.id)
            push!(lines, set.id => push!(lines[set.id], s))
          else
            push!(skippedsets, s)
          end
        end
      end
      if x.cond
        a_f = copy(alive)
        a_f = liveness_(code[1:i-1], rig, lines, a_f, branches, loop, skippedsets)
        merge!(a_b, a_f)
      end
      alive = a_b
      break
    end
  end

  if loop
    return alive
  elseif !isempty(skippedsets)
    println("Warning: Setting locals but values aren't used (Can replace with Drop).")
    println((map(getindex, skippedsets), map(s -> s.i, skippedsets)))
  end
  return alive
end

using GraphLayout
function drawGraph(filename, graph)
  # Output the graph
  am = Matrix(adjacency_matrix(graph))
  loc_x, loc_y = layout_spring_adj(am)
  # draw_layout_adj(am, loc_x, loc_y, filename=filename, arrowlengthfrac=0)
  draw_layout_adj(am, loc_x, loc_y, filename=filename)
end
