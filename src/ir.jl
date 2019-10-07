using IRTools
using IRTools: IR, Variable, isexpr, stmt, argument!, return!, xcall, block!,
  branch!, blocks, insertafter!, arguments, argtypes, isreturn

function locals!(ir::IR)
  locals = argtypes(ir)
  env = Dict{Any,Any}(x => Local(i-1) for (i, x) in enumerate(arguments(ir)))
  rename(x::Variable) = env[x]
  rename(x::Real) = Const(x)
  local!(v, T) = (push!(locals, T); env[v] = Local(length(locals)-1))
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
      end
    end
    empty!(IRTools.branches(b))
  end
  return ir, locals
end

function reloop(ir)
  Block([st.expr for (v, st) in ir])
end

function irfunc(name, ir)
  ir, locals = locals!(ir)
  params = locals[1:length(arguments(ir))]
  locals = locals[length(arguments(ir))+1:end]
  params, locals
  Func(name, params, [f64], locals, reloop(ir))
end
