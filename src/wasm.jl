@enum WType i32=0x7f i64=0x7e f32=0x7d f64=0x7c

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64

WType(::Type{<:Union{Bool,UInt32}}) = i32
WType(::Type{UInt64}) = i64

jltype(x::WType) = [Int32, Int64, Float32, Float64][128-Int(x)]

abstract type Instruction end

struct Const <: Instruction
  typ::WType
  val::UInt64
  opcode::UInt8

  Const(typ,val) = new(typ,val,opcodes["$(typ).const"])
end

Const(x::Union{UInt32,UInt64})   = Const(WType(typeof(x)), UInt64(x))
Const(x::Union{Int64,Int32})     = Const(WType(typeof(x)), reinterpret(UInt64, Int64(x)))
Const(x::Union{Float64,Float32}) = Const(WType(typeof(x)), reinterpret(UInt64, Float64(x)))
Const(x::Bool) = Const(Int32(x))

value(x::Const) = value(x, jltype(x.typ))
value(x::Const, T::Union{Type{Float64},Type{Int64}}) = reinterpret(T, x.val)
value(x::Const, T::Union{Type{Float32},Type{Int32}}) = T(value(x, widen(T)))

struct Nop <: Instruction 
  opcode::UInt8

  Nop() = new(opcodes["nop"])
end

const nop = Nop()

struct Local <: Instruction
  id::Int
  opcode::UInt8

  Local(id) = new(id,opcodes["get_local"])
end

struct SetLocal <: Instruction
  tee::Bool
  id::Int
  opcode::UInt8

  SetLocal(tee,id) = new(tee,id,tee ? opcodes["tee_local"] : opcodes["set_local"])
end

struct Op <: Instruction
  typ::WType
  name::Symbol
  opcode::UInt8

  Op(typ,name) = new(typ,name,opcodes["$(typ).$(name)"])
end

struct Select <: Instruction 
  opcode::UInt8

  Select() = new(opcodes["select"])
end

struct Convert <: Instruction
  to::WType
  from::WType
  name::Symbol
  opcode::UInt8

  Convert(to,from,name) = new(to,from,name,opcodes["$(to).$(name)/$(from)"])
end

struct Block <: Instruction
  body::Vector{Instruction}
  opcode::UInt8

  Block(body) = new(body,opcodes["block"])
end

struct If <: Instruction
  t::Vector{Instruction}
  f::Vector{Instruction}
  opcode::UInt8

  If(t,f) = new(t,f,opcodes["if"])
end

struct Loop <: Instruction
  body::Vector{Instruction}
  opcode::UInt8

  Loop(body) = new(body,opcodes["loop"]) 
end

struct Branch <: Instruction
  cond::Bool
  level::Int
  opcode::UInt8

  Branch(cond,level) = new(cond,level,cond ? opcodes["br_if"] : opcodes["br"])
end

struct Call <: Instruction
  name::Symbol
end

Branch(l::Integer) = Branch(false, l)

struct Return <: Instruction 
  opcode::UInt8

  Return() = new(opcodes["return"])
end

struct Unreachable <: Instruction 
  opcode::UInt8

  Unreachable() = new(opcodes["unreachable"])
end

const unreachable = Unreachable()

struct Func
  name::Symbol
  params::Vector{WType}
  returns::Vector{WType}
  locals::Vector{WType}
  body::Block
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

struct Module
  imports::Vector{Import}
  exports::Vector{Export}
  funcs::Vector{Func}
end

# Write bytecode

Base.write(io::IO, i::Instruction) = write(io,i.opcode)

# Printing

Base.show(io::IO, i::Nop)      = print(io, "nop")
Base.show(io::IO, i::Const)    = print(io, i.typ, ".const ", value(i))
Base.show(io::IO, i::Local)    = print(io, "get_local ", i.id)
Base.show(io::IO, i::SetLocal) = print(io, i.tee ? "tee_local" : "set_local ", i.id)
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
  print(io, "(func \$$(f.name) ")
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
  print(io, "(func \$$(f.name) ")
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
  foreach(p -> printwasm(io, p, 1), m.funcs)
  print(io, ")")
end
