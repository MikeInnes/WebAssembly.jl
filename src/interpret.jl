function interpretwasm(f::Func, s, args)
  params = [convert(jltype(typ), arg) for (typ, arg) in zip(f.params, args)]
  locals = [convert(jltype(typ), 0) for typ in f.locals]
  ms = vcat(params, locals)
  # @show s
  apInstr(f.body, ms, s, 0)

  returns = popn!(ms, length(f.returns))

  # Check to make sure the return types are all correct
  for (rtyp, r) in zip(f.returns, returns)
    if jltype(rtyp) != typeof(r)
      error("ERROR: Return type mismatch")
    end
  end

  return returns
end

# Will eventually be support for more memory, but I don't think it matters
# in the short term.
struct ModuleState
  fs :: Dict{Symbol, Tuple{Int, Function}}
  data :: Data
end

function emptyModuleState()
  ModuleState(Dict(), Data(0,0,[]))
end

function toFunction(f::Func, s)
  return ((args...) -> interpretwasm(f, s, args))
end

function interpret_module(m::Module)
  s = ModuleState(Dict(), isempty(m.data) ? Data(0,0,[]) : m.data[1])
  [(g = toFunction(f, s); s.fs[f.name] = (length(f.params), g); g) for f in m.funcs]
end


function runBody(body, ms, s, level)
  for i in body
    if typeof(i) in [If, Block, Loop, Branch, Return]
      level_ = apInstr(i, ms, s, level + 1)
      level_ <= level && return level_
    elseif typeof(i) in [Call, Op]
      apInstr(i, ms, s)
    else
      apInstr(i, ms)
    end
  end
  return level
end

function load(ms, s, i)
  ptr = pop!(ms)
  num = i.typ == i32 ? 4 : 8
  typ = jltype(i.typ)
  res = reinterpret(typ, s.data.data[ptr + 1:ptr + num])[1]
  push!(ms, res)
end

function popn!(xs, n)
  len = length(xs)
  return splice!(xs, len - n + 1:len)
end

apN(n, f) = (ms, s, i) -> push!(ms, f(popn!(ms, n)...))

# Functions to fix true -> 1 and false -> 0, but that's the case anyway
# function MComp(comparator)
#   return (args...) -> comparator(args...) ? 1 : 0
# end
#
# function cpN(n, f)
#   return apN(n, MComp(f))
# end

unsign(x::Int64) = reinterpret(UInt64, x)
unsign(x::Int32) = reinterpret(UInt32, x)
resign(x::UInt64) = reinterpret(Int64, x)
resign(x::UInt32) = reinterpret(Int32, x)

unsign(x) = x
resign(x) = x

apN_U(n, f) = (ms, s, i) -> push!(ms, resign(f(unsign(popn!(ms, n))...)))

operations =
  Dict(:lt_s   => apN(2, <)
      ,:lt_u   => apN_U(2, <)
      ,:le_s   => apN(2, <=)
      ,:gt_s   => apN(2, >)
      ,:eq     => apN(2, ==)
      ,:ne     => apN(2, !=)
      ,:eqz    => apN(1, x -> x == 0)
      ,:sub    => apN(2, -)
      ,:add    => apN(2, +)
      ,:div    => apN(2, /)
      ,:mul    => apN(2, *)
      ,:ctz    => apN(1, trailing_zeros)
      ,:shr_s  => apN(2, >>)
      ,:shr_u  => apN(2, >>>)
      ,:shl    => apN(2, <<)
      ,:or     => apN(2, |)
      ,:and    => apN(2, &)
      ,:rem_s  => apN(2, rem)
      ,:rem_u  => apN_U(2, rem)
      ,:div_s  => apN(2, div)
      ,:div_u  => apN_U(2, div)
      ,:load   => load
      )

# Level Agnostic Functions

apInstr(i::Nop,         ms) = Void()
apInstr(i::Local,       ms) = push!(ms,ms[i.id + 1]);
apInstr(i::Const,       ms) = push!(ms,value(i));
apInstr(i::Unreachable, ms) = error("Unreachable")
apInstr(i::Convert,     ms) = push!(ms, convert(jltype(i.to), float(pop!(ms))))

apInstr(i::Op,          ms, s) = operations[i.name](ms, s, i)

# apInstr(i::SetLocal,    ms) = i.id + 1 == length(ms) ? ms[i.id + 1] = i.tee ? last(ms) : pop!(ms) : ms
apInstr(i::SetLocal,    ms) = ms[i.id + 1] = i.tee ? last(ms) : pop!(ms)

function apInstr(i::Select, ms)
  arg2, cond = popn!(ms, 2)
  if cond == 0
    pop!(ms)
    push!(ms, arg2)
  end
end

# Level Based Functions

apInstr(i::Return, ms, s, l) = 0
apInstr(i::Branch, ms, s, l) = i.cond && pop!(ms) != 0 || !i.cond ? l - i.level - 1 : l

# Instructions dependent on in scope functions

apInstr(i::Call,   ms, s) = (f = s.fs[i.name]; push!(ms, f[2]((popn!(ms, f[1]))...)...))

apInstr(i::If,     ms, s, l) = pop!(ms) != 0 ? runBody(i.t, ms, s, l) : runBody(i.f, ms, s, l)
apInstr(i::Block,  ms, s, l) = runBody(i.body, ms, s, l)
function apInstr(loop::Loop, ms, s, level)
  level_ = level
  while level_ == level
    level_ = runBody(loop.body, ms, s, level)
  end
  return level_
end
