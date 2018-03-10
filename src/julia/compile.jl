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

binary_ops = [
  (:add_int, :add),
  (:sub_int, :sub),
  (:(===), :eq),
  (:slt_int, :lt_s),
  (:sle_int, :le_s)]

for (j, w) in binary_ops
  wasmfuncs[GlobalRef(Base, j)] = function (A,B)
    @assert A == B
    Op(WType(A), w)
  end
end

wasmfuncs[GlobalRef(Base, :not_int)] = function (A)
  Op(WType(A), :eqz)
end

wasmfuncs[GlobalRef(Base, :select_value)] = function (c, t, f)
  Select()
end

wasmfunc(f, xs...) = wasmfuncs[f](xs...)

isprimitive(x) = false
isprimitive(x::GlobalRef) =
  getfield(x.mod, x.name) isa Core.IntrinsicFunction ||
  getfield(x.mod, x.name) isa Core.Builtin

function wasmcalls(c::CodeInfo, code)
  map(code) do x
    prewalk(x) do x
      if (isexpr(x, :call) && isprimitive(x.args[1]))
        Expr(:call,
             wasmfunc(x.args[1], exprtype.(c, x.args[2:end])...),
             x.args[2:end]...)
      elseif isexpr(x, :(=)) && x.args[1] isa SlotNumber
        Expr(:call, SetLocal(false, x.args[1].id-1), x.args[2])
      elseif x isa SlotNumber
        Local(x.id-2)
      elseif isexpr(x, :return)
        Expr(:call, Return(), x.args[1])
      else
        x
      end
    end
  end
end

function lower(c::CodeInfo)
  code = wasmcalls(c, inlinessa(c.code))
  restructure(code)
end

# Convert to WASM instructions

function towasm(x, is = Instruction[])
  if x isa Instruction
    push!(is, x)
  elseif isexpr(x, :block)
    foreach(x -> towasm(x, is), x.args)
  elseif isexpr(x, :call) && x.args[1] isa Instruction
    foreach(x -> towasm(x, is), x.args[2:end])
    push!(is, x.args[1])
  elseif isexpr(x, :if)
    towasm(x.args[1], is)
    push!(is, If(towasm(x.args[1]), towasm(x.args[2])))
  elseif x isa Number
    push!(is, Const(x))
  elseif x isa LineNumberNode
  else
    error("Can't convert to wasm: $x")
  end
  return is
end

function code_wasm(ex, A)
  cinfo, R = code_typed(ex, A)[1]
  body = cinfo |> lower |> towasm
  Func([WType(T) for T in A.parameters],[WType(R)],[],body)
end
