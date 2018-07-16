function interpretwasm(f::Func, s, args)
  params = [convert(jltype(typ), arg) for (typ, arg) in zip(f.params, args)]
  locals = [convert(jltype(typ), 0) for typ in f.locals]
  ms = Vector{Integer}(vcat(params, locals))

  apInstr(f.body, ms, 0, s)

  returns = popn!(ms, length(f.returns))

  # Check to make sure the return types are all correct
  for (rtyp, r) in zip(f.returns, returns)
    if jltype(rtyp) != typeof(r)
      # @show r
      if r isa Bool && rtyp == i32
        continue
      else
        error("ERROR: Return type mismatch")
      end
    end
  end

  return returns
end

# The state of the module during execution
struct State
  mem :: Vector{Vector{UInt8}}        # Linear Memory
  max :: Vector{Union{UInt32, Nothing}}  # Max size of each linear memory
  fs  :: Dict{Symbol, Any}            # Function space
  gs  :: Vector{Tuple{Bool, Integer}} # Global Variables, bool determines mutability.
end

const page_size = 65536

# Initialise the state with data from data section and create functions, calls
# the start function.
function State(m::Module)
  mem = [zeros(UInt8, m.min * page_size) for m in m.mems]
  max = [m.max for m in m.mems]
  for d in m.data
    mem[d.memidx+1][d.offset+1:d.offset+length(d.data)] = d.data
  end
  gs = [(g.mut, jltype(g.typ)(g.init)) for g in m.globals]
  s = State(mem, max, Dict(), gs)
  for f in m.funcs
    s.fs[f.name] = length(f.params), toFunction(f, s)
  end
  if m.start != nothing
    s.fs[m.start][2]()
  end
  return s
end


function toFunction(f::Func, s)
  return ((args...) -> interpretwasm(f, s, args))
end

function interpret_module(m::Module)
  s = State(m)
  # efs = filter(e->e.typ==:func, m.exports)
  # [s.fs[e.internalname][2] for e in efs]
  return [s.fs[f.name][2] for f in m.funcs]
end

# Returns dictionary of exported functions
function interpret_module_dict(m::Module)
  s = State(m)
  @show keys(s.fs)
  efs = filter(e->e.typ==:func, m.exports)
  return Dict(e.name => (xs...) -> s.fs[e.internalname][2](xs...)[1] for e in efs)
end

function runBody(body, ms, level, s, checkbranch=false)
  for i in body
    if typeof(i) in [Branch, Return]
      level_ = apInstr(i, ms, level + 1)
      level_ <= level && return checkbranch ? (level_, i isa Branch) : level_
    elseif typeof(i) in [If, Block, Loop]
      level_ = apInstr(i, ms, level + 1, s)
      level_ <= level && return checkbranch ? (level_, false) : level_
    elseif typeof(i) ∈ [Call, MemoryOp, GetGlobal, SetGlobal, MemoryUtility]
      apInstr(i, ms, s)
    else
      apInstr(i, ms)
    end
  end
  return checkbranch ? (level, false) : level
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
      ,:le_u   => apN_U(2, <=)
      ,:gt_s   => apN(2, >)
      ,:gt_u   => apN_U(2, >)
      ,:ge_s   => apN(2, >=)
      ,:ge_u   => apN_U(2, >=)
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
      ,:xor    => apN(2, xor)
      ,:and    => apN(2, &)
      ,:rem_s  => apN(2, rem)
      ,:rem_u  => apN_U(2, rem)
      ,:div_s  => apN(2, div)
      ,:div_u  => apN_U(2, div)
      ,:clz    => apN(1,leading_zeros)
      )

# Level Agnostic Functions

apInstr(i::Nop,         ms) = nothing
apInstr(i::Local,       ms) = push!(ms,ms[i.id + 1]);
apInstr(i::Const,       ms) = push!(ms,value(i));
apInstr(i::Unreachable, ms) = error("Unreachable")
apInstr(i::Drop, ms)        = pop!(ms)
apInstr(i::Convert,     ms) = push!(ms, convert(jltype(i.to), float(pop!(ms))))
apInstr(i::Op,          ms) = operations[i.name](ms)
# apInstr(i::Op,          ms) = ((@show ms[end-min(5, length(ms)-1):end], i.name); @show operations[i.name](ms))
apInstr(i::GetGlobal,   ms, s) = push!(ms, s.gs[i.id + 1][2])
apInstr(i::SetGlobal,   ms, s) = s.gs[i.id + 1] = (s.gs[i.id + 1][1], s.gs[i.id + 1][1] ? pop!(ms) : error("Can't set immutable global."))
function apInstr(i::MemoryUtility, ms, s)
  if i.name == :current_memory
    push!(ms, Int32(length(s.mem[1])/page_size))
  elseif i.name == :grow_memory
    println("this isn't being called again")
    num_pages = pop!(ms)
    current_memory = Int32(length(s.mem[1])/page_size)
    if s.max[1] != nothing && num_pages + current_memory > s.max[1]
      push!(ms, -1)
    else
      println(Int32(length(s.mem[1])/page_size))

      push!(ms, current_memory)
      s.mem[1] = vcat(s.mem[1], zeros(UInt8, num_pages * page_size))
      # push!(s.mem[1], zeros(UInt8, num_pages * page_size)...)
      println(Int32(length(s.mem[1])/page_size))
    end
  else
    error("nope")
  end
end

function apInstr(i::MemoryOp, ms, s)
  typ = jltype(i.typ)
  if i.name == :load
    address = pop!(ms)
    effective_address = address + i.offset
    bs = s.mem[1][effective_address+1:effective_address+sizeof(i.store_type)]
    # if i.bytes == sizeof(typ)
      # @show reinterpret(typ, bs)[1]
      # push!(ms,reinterpret(typ, bs)[1])
    # else
      # error("Sign extension not yet done")
      # if i.signed == true
      b = reinterpret(i.store_type, bs)[1]
      x = Base.sext_int(typ, b)
      push!(ms, x)
      typeof(ms[end]) == typ || error("that didn't work")
        # Base.sext_int(typ, )
    # end
  else
    value = pop!(ms)
    address = pop!(ms)
    effective_address = address + i.offset
    # if unsigned(i.store_type) == unsigned(typ)
      # s.mem[1][effective_address+1:effective_address+sizeof(i.store_type)] = reinterpret(UInt8, [value])
    # else
      # error("wrapping not yet done")
    s.mem[1][effective_address+1:effective_address+sizeof(i.store_type)] = reinterpret(UInt8, [Base.trunc_int(i.store_type, value)])
    # end
  end
end

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

apN(n, f, ms) = push!(ms, f(popn!(ms, n)...)...)
apInstr(i::Call,   ms, s) = apN(s.fs[i.name]...,ms)

apInstr(i::If,     ms, l, s) = pop!(ms) != 0 ? runBody(i.t, ms, l, s) : runBody(i.f, ms, l, s)
apInstr(i::Block,  ms, l, s) = runBody(i.body, ms, l, s)
function apInstr(loop::Loop, ms, level, s)
  level_ = level
  isbranched = true
  while isbranched && level_ == level
    level_, isbranched = runBody(loop.body, ms, level, s, true)
  end
  return level_
end
