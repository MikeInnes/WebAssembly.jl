@enum WType i32 i64 f32 f64

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64
WType(t::Type{Array{T}}) where T = i32 # i64 in wasm64
WType(t::Type{Array{T, N}}) where T where N = i32

WType(::Type{<:Union{Bool,UInt32}}) = i32
WType(::Type{UInt64}) = i64

jltype(x::WType) = [Int32, Int64, Float32, Float64][Int(x)+1]

abstract type Instruction end

import Base.parse
parse(t::Type{WType}, s) = Dict(zip(map(string, instances(WType)), instances(WType)))[s]

struct Const <: Instruction
  typ::WType
  val::UInt64
end

Const(x::Union{UInt32,UInt64})   = Const(WType(typeof(x)), UInt64(x))
Const(x::Union{Int64,Int32})     = Const(WType(typeof(x)), reinterpret(UInt64, Int64(x)))
Const(x::Union{Float64,Float32}) = Const(WType(typeof(x)), reinterpret(UInt64, Float64(x)))
Const(x::Bool) = Const(Int32(x))

value(x::Const) = value(x, jltype(x.typ))
value(x::Const, T::Union{Type{Float64},Type{Int64}}) = reinterpret(T, x.val)
value(x::Const, T::Union{Type{Float32},Type{Int32}}) = T(value(x, widen(T)))

struct Nop <: Instruction end

const nop = Nop()

struct Local <: Instruction
  id::Int
end

struct SetLocal <: Instruction
  tee::Bool
  id::Int
end

struct GetGlobal <: Instruction
  id::Int
end

struct SetGlobal <: Instruction
  id::Int
end

struct Op <: Instruction
  typ::WType
  name::Symbol
end

struct MemoryOp <: Instruction
  typ :: WType
  name :: Symbol # :load or :store
  store_type # E.g UInt8 for :load8_u
  offset :: UInt # Effective address = value + offset
  alignment :: UInt # Power of 2
end
# MemoryOp(op::Op) = MemoryOp(op, 0, 0)
# MemoryOp(typ :: WType)
MemoryOp(typ::WType, name::Symbol, offset::Unsigned, alignment::Unsigned) = MemoryOp(typ, name, jltype(typ), offset, alignment)
# MemoryOp(typ::WType, name::Symbol, store_type, offset::Unsigned, alignment::Unsigned) = MemoryOp(typ, name, store_type, offset, alignment)
# MemoryOp(typ::WType, name::Symbol, bytes::Unsigned, offset::Unsigned, alignment::Unsigned) = MemoryOp(typ, name, bytes, nothing, offset, alignment)

struct MemoryUtility <: Instruction
  name :: Symbol # :current_memory, :grow_memory
  reserved :: Bool # Reserved but currently unused in MVP
end
MemoryUtility(name :: Symbol) = MemoryUtility(name, false)

struct Select <: Instruction end

struct Convert <: Instruction
  to::WType
  from::WType
  name::Symbol
end

struct Block <: Instruction
  body::Vector{Instruction}

  result::Union{WType, Nothing}
end
Block(body) = Block(body, nothing)

struct If <: Instruction
  t::Vector{Instruction}
  f::Vector{Instruction}

  result::Union{WType, Nothing}
end
If(t,f) = If(t, f, nothing)

struct Loop <: Instruction
  body::Vector{Instruction}
  result::Union{WType, Nothing}
end
Loop(body) = Loop(body, nothing)

struct Branch <: Instruction
  cond::Bool
  level::Int
end

struct Call <: Instruction
  name::Symbol
end

Branch(l::Integer) = Branch(false, l)

struct Return <: Instruction end

struct Unreachable <: Instruction end

struct Drop <: Instruction end

const unreachable = Unreachable()

struct FuncType
  params::Vector{WType}
  returns::Vector{WType}
end

struct Func
  name::Symbol
  params::Vector{WType}
  returns::Vector{WType}
  locals::Vector{WType}
  body::Block
end

struct Table
  # TODO
end

struct Mem
  name::Symbol
  min::UInt32
  max::Union{UInt32, Nothing}
end

struct Global
  typ::WType
  mut::Bool
  init::Integer
end

struct Elem
  # TODO
end

struct Data
  memidx::UInt32
  offset::UInt32
  data::Vector{UInt8}
end

struct Import
  mod::Symbol
  field_str::Symbol
  name::Symbol
  typ::Symbol   # :func, :table, :memory, :global
  x::Union{FuncType} # Eventually support the other types
end

struct Export
  name::Symbol
  internalname::Symbol
  typ::Symbol   # :func, :table, :memory, :global
end

struct Module
  types::Vector{FuncType}
  funcs::Vector{Func}
  tables::Vector{Table}
  mems::Vector{Mem}       # Only one of these is allowed right now
  globals::Vector{Global}
  elem::Vector{Elem}
  data::Vector{Data}
  start::Union{Symbol, Nothing}
  imports::Vector{Import}
  exports::Vector{Export}
end

# Printing

Base.show(io::IO, i::Nop)      = print(io, "nop")
Base.show(io::IO, i::Const)    = print(io, i.typ, ".const ", value(i))
Base.show(io::IO, i::Local)    = print(io, "get_local ", i.id)
Base.show(io::IO, i::SetLocal) = print(io, i.tee ? "tee_local " : "set_local ", i.id)
Base.show(io::IO, i::Op)       = print(io, i.typ, ".", i.name)
Base.show(io::IO, i::Call)     = print(io, "call \$", i.name)
Base.show(io::IO, i::Convert)  = print(io, i.to, ".", i.name, "/", i.from)
Base.show(io::IO, i::Select)   = print(io, "select")
Base.show(io::IO, i::Branch)   = print(io, i.cond ? "br_if " : "br ", i.level)
Base.show(io::IO, i::Return)   = print(io, "return")
Base.show(io::IO, i::Unreachable) = print(io, "unreachable")
Base.show(io::IO, i::Drop)     = print(io, "drop")

printwasm(io, x, level) = show(io, x)

function printwasm_(io, xs, level)
  for x in xs
    print(io, "\n", "  "^(level))
    print(io, "(")
    printwasm(io, x, level)
    print(io, ")")
  end
end

function printwasm(io, x::If, level)
  level += 1
  print(io, "if")
  if !isempty(x.t)
    print(io, "\n", "  "^level, "(then")
    printwasm_(io, x.t, level+1)
  end
  if !isempty(x.f)
    print(io, ")\n", "  "^level, "(else")
    printwasm_(io, x.f, level+1)
  end
  print(io, ")")
end

function printwasm(io, x::Block, level)
  print(io, "block")
  printwasm_(io, x.body, level+1)
end

function printwasm(io, x::Loop, level)
  print(io, "loop")
  printwasm_(io, x.body, level+1)
end

Base.show(io::IO, i::Union{Block,Loop,If}) = printwasm(io, i, 0)

function printwasm(io, x::Mem, level)
  print(io, "\n", "  "^(level))
  print(io, "(memory $(x.min))")    # TODO: add x.max
end

function printwasm(io, x::Data, level)
  print(io, "\n", "  "^(level))
  print(io, """(data (i32.const $(x.offset)) "$(String(x.data))"))""")
end

function printwasm(io, x::Export, level)
  print(io, "\n", "  "^(level))
  print(io, "(export \"$(x.name)\" ($(x.typ) \$$(x.internalname)))")
end

function printwasm(io, x::Import, level)
  print(io, "\n", "  "^(level))
  print(io, "(import \"$(x.mod)\" \"$(x.field_str)\" ($(x.typ) \$$(x.name)")
  if x.typ == :func && length(x.x.params) > 0
    foreach(p -> print(io, " (param $p)"), x.x.params)
    foreach(p -> print(io, " (result $p)"), x.x.returns)
  end
  print(io, "))")
end

function Base.show(io::IO, f::Func)
  print(io, "(func \$$(f.name)")
  foreach(p -> print(io, " (param $p)"), f.params)
  foreach(p -> print(io, " (result $p)"), f.returns)
  if !isempty(f.locals)
    print(io, "\n ")
    foreach(p -> print(io, " (local $p)"), f.locals)
  end
  printwasm_(io, f.body.body, 1)
  print(io, ")")
end

function printwasm(io::IO, f::Func, level)
  print(io, "\n", "  "^(level))
  print(io, "(func \$$(f.name)")
  foreach(p -> print(io, " (param $p)"), f.params)
  foreach(p -> print(io, " (result $p)"), f.returns)
  if !isempty(f.locals)
    print(io, "\n ")
    foreach(p -> print(io, " (local $p)"), f.locals)
  end
  printwasm_(io, f.body.body, level + 1)
  print(io, ")")
end

function Base.show(io::IO, m::Module)
  print(io, "(module")
  foreach(p -> printwasm(io, p, 1), m.imports)
  foreach(p -> printwasm(io, p, 1), m.exports)
  foreach(p -> printwasm(io, p, 1), m.mems)
  foreach(p -> printwasm(io, p, 1), m.data)
  foreach(p -> printwasm(io, p, 1), m.funcs)
  print(io, ")")
end
