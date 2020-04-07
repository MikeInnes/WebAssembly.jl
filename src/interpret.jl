function interpretwasm(f::Func, fs, args)
  params = [convert(jltype(typ), arg) for (typ, arg) in zip(f.params, args)]
  locals = [convert(jltype(typ), 0) for typ in f.locals]
  ms = vcat(params, locals)

  apInstr(f.body, ms, 0, fs)

  returns = popn!(ms, length(f.result))

  # Check to make sure the return types are all correct
  for (rtyp, r) in zip(f.result, returns)
    if jltype(rtyp) != typeof(r)
      error("ERROR: Return type mismatch")
    end
  end

  return returns
end

function toFunction(f::Func, fs)
  return ((args...) -> interpretwasm(f, fs, args))
end

function interpret_module(m::Module)
  d = Dict()
  [(g = toFunction(f, d); d[f.name] = (length(f.params), g); g) for f in m.funcs]
end


function runBody(body, ms, level, fs)
  for i in body
    if typeof(i) in [Branch, Return]
      level_ = apInstr(i, ms, level + 1)
      level_ <= level && return level_
    elseif typeof(i) in [If, Block, Loop]
      level_ = apInstr(i, ms, level + 1, fs)
      level_ <= level && return level_
    elseif i isa Call
      apInstr(i, ms, level + 1, fs)
    else
      apInstr(i, ms)
    end
  end
  return level
end

function popn!(xs, n)
  len = length(xs)
  return splice!(xs, len - n + 1:len)
end

apN(n, f) = ms -> push!(ms, f(popn!(ms, n)...))

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

apN_U(n, f) = ms -> push!(ms, resign(f(unsign(popn!(ms, n))...)))

operations =
  Dict(:lt_s   => apN(2, <)
      ,:lt_u   => apN_U(2, <)
      ,:le_s   => apN(2, <=)
      ,:gt_s   => apN(2, >)
      ,:eq     => apN(2, ==)
      ,:ne     => apN(2, !=)
      ,:gt     => apN(2, >)
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
      )

# Level Agnostic Functions

apInstr(i::Nop,         ms) = nothing
apInstr(i::Local,       ms) = push!(ms,ms[i.id + 1]);
apInstr(i::Const,       ms) = push!(ms,value(i));
apInstr(i::Unreachable, ms) = error("Unreachable")
apInstr(i::Convert,     ms) = push!(ms, convert(jltype(i.to), float(pop!(ms))))
apInstr(i::Op,          ms) = operations[Symbol(split(string(i.name), ".")[2])](ms)

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

apInstr(i::Return, ms, l) = 0
apInstr(i::Branch, ms, l) = i.cond && pop!(ms) != 0 || !i.cond ? l - i.level - 1 : l

# Instructions dependent on in scope functions

apInstr(i::Call,   ms, l, fs) = (f = fs[i.name]; push!(ms, f[2]((popn!(ms, f[1]))...)...))

apInstr(i::If,     ms, l, fs) = pop!(ms) != 0 ? runBody(i.t, ms, l, fs) : runBody(i.f, ms, l, fs)
apInstr(i::Block,  ms, l, fs) = runBody(i.body, ms, l, fs)
function apInstr(loop::Loop, ms, level, fs)
  level_ = level
  while level_ == level
    level_ = runBody(loop.body, ms, level, fs)
  end
  return level_
end
