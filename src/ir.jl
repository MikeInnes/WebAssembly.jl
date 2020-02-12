using IRTools
using IRTools: IR, CFG, Variable, isexpr, stmt, argument!, return!, xcall, block!,
  branch!, blocks, insertafter!, arguments, argtypes, isreturn, stackify, isconditional
using Base: @get!

struct WTuple
  parts::Vector{WType}
end

WTuple(Ts::WType...) = WTuple([Ts...])

function Base.show(io::IO, t::WTuple)
  print(io, "(")
  join(io, t.parts, ", ")
  print(io, ")")
end

function locals!(ir::IR)
  locals = argtypes(ir)
  ret = []
  env = Dict{Any,Any}(x => Local(i-1) for (i, x) in enumerate(arguments(ir)))
  tuples = Dict()
  rename(x::Variable) = env[x]
  rename(x::Real) = Const(x)
  rename(x::Union{Const,Local}) = x
  ltype(x) = rename(x) isa Const ? rename(x).typ : locals[rename(x).id+1]
  local!(T) = (push!(locals, T); Local(length(locals)-1))
  local!(v, T) = @get!(env, v, local!(T))
  for b in blocks(ir)
    for (v, st) in b
      ex = st.expr
      if ex isa Variable
        delete!(ir, v)
        env[v] = rename(ex)
      elseif !isexpr(ex)
        delete!(ir, v)
        env[v] = Const(ex)
      elseif isexpr(ex, :call)
        for arg in ex.args[2:end]
          insert!(ir, v, rename(arg))
        end
        ir[v] = ex.args[1]::Instruction
        if st.type isa WTuple
          tuples[v] = [(l = local!(T); insertafter!(ir, v, SetLocal(false, l.id)); l)
                       for T in st.type.parts]
        else
          insertafter!(ir, v, SetLocal(false, local!(v, st.type).id))
        end
      elseif isexpr(ex, :tuple)
        tuples[v] = rename.(ex.args)
        delete!(ir, v)
      elseif isexpr(ex, :ref)
        env[v] = tuples[ex.args[1]][ex.args[2]]
        delete!(ir, v)
      else
        error("Unrecognised wasm expression $ex")
      end
    end
    for br in IRTools.branches(b)
      if isreturn(br)
        args = haskey(tuples, arguments(br)[1]) ? tuples[arguments(br)[1]] : arguments(br)
        ret = ltype.(args)
        for arg in args
          push!(b, rename(arg))
        end
        push!(b, Return())
      else
        for (x, y) in zip(arguments(br), arguments(IRTools.block(ir, br.block)))
          push!(b, rename(x))
          push!(b, SetLocal(false, local!(y, ltype(x)).id))
        end
        isconditional(br) && push!(b, rename(br.condition))
        push!(b, Branch(isconditional(br), br.block))
      end
    end
    empty!(IRTools.branches(b))
  end
  return ir, locals, ret
end

isbackedge((from, to)) = to <= from

function reloop(ir, cfg)
  scopes = []
  targets = []
  forw, back = stackify(cfg)
  push!(scopes, Block([]))
  block!() = (bl = Block([]); push!(scopes[1].body, bl); pushfirst!(scopes, bl))
  loop!() = (bl = Loop([]); push!(scopes[1].body, bl); pushfirst!(scopes, bl))
  for b in blocks(ir)
    any(br -> br[2] == b.id, forw) && (popfirst!(scopes); popfirst!(targets))
    brs = vcat(filter(br -> br[1] == b.id, forw), filter(br -> br[2] == b.id, back))
    sort!(brs, by = br -> isbackedge(br) ? (br[1], 1) : (br[2], -1), rev = true)
    for br in brs
      isbackedge(br) ? loop!() : block!()
      pushfirst!(targets, br[2])
    end
    for (v, st) in b
      if st.expr isa Branch
        st.expr.cond && push!(scopes[1].body, i32.eqz)
        target = findfirst(b -> b == st.expr.level, targets)-1
        push!(scopes[1].body, Branch(st.expr.cond, target))
      else
        push!(scopes[1].body, st.expr)
      end
    end
    any(br -> br[1] == b.id, back) && (popfirst!(scopes); popfirst!(targets))
  end
  return scopes[end]
end

function irfunc(name, ir)
  cfg = CFG(ir)
  ir, locals, ret = locals!(ir)
  params = locals[1:length(arguments(ir))]
  locals = locals[length(arguments(ir))+1:end]
  Func(name, params, ret, locals, reloop(ir, cfg))
end
