walk(x, inner, outer) = outer(x)

function walk(x::Expr, inner, outer)
  y = Expr(x.head, map(inner, x.args)...)
  y.typ = x.typ
  return outer(y)
end

prepostwalk(f, g, x) = walk(f(x), x -> prepostwalk(f, g, x), g)
prewalk(f, x)  = prepostwalk(f, identity, x)
postwalk(f, x) = prepostwalk(identity, f, x)

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

iscontrol(ex) = isexpr(ex, :while) || isexpr(ex, :if)

function control(ex)
  stack = []
  breaks = []
  pre = ex -> (iscontrol(ex) && (push!(stack, ex.head); push!(breaks, false)); ex)
  post = function (ex)
    iscontrol(ex) && (pop!(stack); pop!(breaks))
    if isexpr(ex, :continue)
      label = findfirst(reverse(stack), :while)
      return Branch(label-1)
    elseif isexpr(ex, :break)
      label = findfirst(reverse(stack), :while)
      breaks[length(stack)+1-label] = true
      return Branch(label)
    end
    ex
  end
  prepostwalk(pre, post, ex)
end

function lower(c::CodeInfo)
  code = wasmcalls(c, inlinessa(c.code))
  code |> restructure |> control
end

# Convert to WASM instructions

towasm_(xs, is = Instruction[]) = (foreach(x -> towasm(x, is), xs); is)

function towasm(x, is = Instruction[])
  if x isa Instruction
    push!(is, x)
  elseif isexpr(x, :block)
    push!(is, Block(towasm_(x.args)))
  elseif isexpr(x, :call) && x.args[1] isa Instruction
    foreach(x -> towasm(x, is), x.args[2:end])
    push!(is, x.args[1])
  elseif isexpr(x, :if)
    towasm(x.args[1], is)
    push!(is, If(towasm_(x.args[2].args), towasm_(x.args[3].args)))
  elseif isexpr(x, :while)
    push!(is, Block([Loop(towasm_(x.args[2].args))]))
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
  body = towasm_(lower(cinfo).args)
  Func([WType(T) for T in A.parameters],
       [WType(R)],
       [WType(P) for P in cinfo.slottypes[2:end]],
       body)
end
