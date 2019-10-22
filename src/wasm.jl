@enum WType i32 i64 f32 f64

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64

WType(::Type{<:Union{Bool,UInt32}}) = i32
WType(::Type{UInt64}) = i64

WType(T::WType) = T

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

struct Op <: Instruction
  typ::WType
  name::Symbol
end

Base.getproperty(x::WType, op::Symbol) = Op(x, op)

struct Select <: Instruction end

struct Convert <: Instruction
  to::WType
  from::WType
  name::Symbol
end

struct Block <: Instruction
  body::Vector{Instruction}
end

struct If <: Instruction
  t::Vector{Instruction}
  f::Vector{Instruction}
end

struct Loop <: Instruction
  body::Vector{Instruction}
end

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

const unreachable = Unreachable()

struct FuncType
  # TODO
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
  max::Union{UInt32,Nothing}
end

struct Global
  # TODO
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
  name::Symbol
  typ::Symbol   # :func, :table, :memory, :global
  params::Vector{WType}
  returntype::WType
end

struct Export
  name::Symbol
  internalname::Symbol
  typ::Symbol   # :func, :table, :memory, :global
end

# TODO perhaps split this into sections
struct Module
  types::Vector{FuncType}
  funcs::Vector{Func}
  tables::Vector{Table}
  mems::Vector{Mem}       # Only one of these is allowed right now
  globals::Vector{Global}
  elem::Vector{Elem}
  data::Vector{Data}
  start::Ref{Int}
  imports::Vector{Import}
  exports::Vector{Export}
end

Module(; types = [], funcs = [], tables = [], mems = [], globals = [], elem = [], data = [], start = Ref(0), imports = [], exports = []) =
  Module(types, funcs, tables, mems, globals, elem, data, start, imports, exports)

# Printing

Base.show(io::IO, i::Nop)      = print(io, "nop")
Base.show(io::IO, i::Const)    = print(io, i.typ, ".const ", value(i))
Base.show(io::IO, i::Local)    = print(io, "local.get ", i.id)
Base.show(io::IO, i::SetLocal) = print(io, i.tee ? "local.tee " : "local.set ", i.id)
Base.show(io::IO, i::Op)       = print(io, i.typ, ".", i.name)
Base.show(io::IO, i::Call)     = print(io, "call \$", i.name)
Base.show(io::IO, i::Convert)  = print(io, i.to, ".", i.name, "/", i.from)
Base.show(io::IO, i::Select)   = print(io, "select")
Base.show(io::IO, i::Branch)   = print(io, i.cond ? "br_if " : "br ", i.level)
Base.show(io::IO, i::Return)   = print(io, "return")
Base.show(io::IO, i::Unreachable) = print(io, "unreachable")

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
  print(io, "(import \"$(x.mod)\" \"$(x.name)\" ($(x.typ) \$$(x.mod)_$(x.name)")
  if x.typ == :func && length(x.params) > 0
    print(io, " (param")
    foreach(p -> print(io, " $p"), x.params)
    print(io, ")")
    print(io, " (result ", x.returntype, ")")
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
