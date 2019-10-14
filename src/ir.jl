using IRTools
using IRTools: IR, CFG, Variable, isexpr, stmt, argument!, return!, xcall, block!,
  branch!, blocks, insertafter!, arguments, argtypes, isreturn, stackify, isconditional

function locals!(ir::IR)
  locals = argtypes(ir)
  env = Dict{Any,Any}(x => Local(i-1) for (i, x) in enumerate(arguments(ir)))
  rename(x::Variable) = env[x]
  rename(x::Real) = Const(x)
  ltype(x) = rename(x) isa Const ? rename(x).typ : locals[rename(x).id+1]
  local!(v, T) = haskey(env, v) ? env[v] :
    (push!(locals, T); env[v] = Local(length(locals)-1))
  for b in blocks(ir)
    for (v, st) in b
      isexpr(st.expr) || (delete!(ir, v); env[v] = Const(st.expr); continue)
      for arg in st.expr.args[2:end]
        insert!(ir, v, rename(arg))
      end
      ir[v] = st.expr.args[1]
      insertafter!(ir, v, SetLocal(false, local!(v, st.type).id))
    end
    for br in IRTools.branches(b)
      if isreturn(br)
        push!(b, env[arguments(br)[1]])
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
  return ir, locals
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
  ir, locals = locals!(ir)
  params = locals[1:length(arguments(ir))]
  locals = locals[length(arguments(ir))+1:end]
  Func(name, params, [f64], locals, reloop(ir, cfg))
end
