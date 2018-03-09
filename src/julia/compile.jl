walk(x, inner, outer) = outer(x)

function walk(x::Expr, inner, outer)
  y = Expr(x.head, map(inner, x.args)...)
  y.typ = x.typ
  return outer(y)
end

postwalk(f, x) = walk(x, x -> postwalk(f, x), f)
prewalk(f, x)  = walk(f(x), x -> prewalk(f, x), identity)

exprtype(code::CodeInfo, x) = typeof(x)
exprtype(code::CodeInfo, x::Expr) = x.typ
exprtype(code::CodeInfo, x::QuoteNode) = typeof(x.value)
exprtype(code::CodeInfo, x::SSAValue) = code.ssavaluetypes[x.id+1]
exprtype(code::CodeInfo, x::SlotNumber) = code.slottypes[x.id]

# We don't want SSAValues on a stack machine.
# If it's only used once, just inline it.
# If more than once, we have to turn it into a local.

function ssacounts(code)
  counts = Dict{SSAValue,Any}()
  inc(x) = counts[x] = get(counts, x, 0) + 1
  for c in code
    isexpr(c, :(=)) && (c = c.args[2])
    prewalk(x -> (x isa SSAValue && inc(x); x), c)
  end
  return counts
end

apply_rhs(f, x) =
    isexpr(x, :(=)) ?
      Expr(:(=), x.args[1], f(x.args[2])) :
      f(x)

function inlinessa(code)
  counts = ssacounts(code)
  values = Dict{SSAValue,Any}()
  code′ = []
  for c in code
    c = apply_rhs(c) do x
      prewalk(x -> get(values, x, x), x)
    end
    isexpr(c, :(=)) && c.args[1] isa SSAValue && counts[c.args[1]] == 1 ?
      (values[c.args[1]] = c.args[2]) :
      push!(code′, c)
  end
  return code′
end

# Julia -> WASM expression

wasmfuncs = Dict()

wasmfuncs[GlobalRef(Base, :add_int)] = function (A,B)
  @assert A == B
  BinaryOp(WType(A), :add)
end

wasmfunc(f, xs...) = wasmfuncs[f](xs...)

isprimitive(x) = false
isprimitive(x::GlobalRef) = getfield(x.mod, x.name) isa Core.IntrinsicFunction

function wasmcalls(c::CodeInfo, code)
  map(code) do x
    prewalk(x) do x
      if (isexpr(x, :call) && isprimitive(x.args[1]))
        Expr(:call,
             wasmfunc(x.args[1], exprtype.(c, x.args[2:end])...),
             x.args[2:end]...)
      elseif isexpr(x, :(=)) && x.args[1] isa SlotNumber
        Expr(:call, SetLocal(false, x.args[1].id-1), x.args[2])
      elseif x isa Real
        Const(x)
      elseif x isa SlotNumber
        Local(x.id-2)
      elseif isexpr(x, :return)
        :($(x.args[1]); $(Return()))
      else
        x
      end
    end
  end
end

function stackify(code, ex)
  if isexpr(ex, :return)
    stackify(code, ex.args[1])
    push!(code, :(return))
  elseif isexpr(ex, :gotoifnot)
    stackify(code, ex.args[1])
    push!(code, Expr(:gotoifnot, :(↑), ex.args[2]))
  elseif isexpr(ex, :call)
    foreach(a -> stackify(code, a), ex.args[2:end])
    push!(code, ex.args[1])
  elseif isexpr(ex, :block)
    foreach(x -> stackify(code, x), ex.args)
  else
    push!(code, ex)
  end
end

function stackify(code)
  code′ = []
  for c in code
    stackify(code′, c)
  end
  return code′
end

function lower(c::CodeInfo)
  code = wasmcalls(c, inlinessa(c.code))
  code |> stackify |> restructure
end

# Convert to WASM instructions

function towasm(ex, is = Instruction[])
  @assert isexpr(ex, :block)
  for x in ex.args
    if x isa Instruction
      push!(is, x)
    elseif isexpr(x, :block)
      towasm(x, is)
    else
      error("Can't convert $x to wasm")
    end
  end
  return is
end

function code_wasm(ex, A)
  cinfo, R = code_typed(ex, A)[1]
  body = cinfo |> lower |> towasm
  Func([WType(T) for T in A.parameters],[WType(R)],[],body)
end
