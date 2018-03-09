@enum WType i32 i64 f32 f64

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64

jltype(x::WType) =
  x == i32 ? Int32 :
  x == i64 ? Int64 :
  x == f32 ? Float32 :
  x == f64 ? Float64 :
  error("Unrecognised WType $x")

struct Const
  typ::WType
  val::UInt64
end

Const(x::Union{Int64,Int32})     = Const(WType(typeof(x)), reinterpret(UInt64, Int64(x)))
Const(x::Union{Float64,Float32}) = Const(WType(typeof(x)), reinterpret(UInt64, Float64(x)))

struct Local
  id::Int
end

struct Binary
  typ::WType
  name::Symbol
end

const Instruction = Union{Const,Local,Binary}

struct Func
  params::Vector{WType}
  returns::Vector{WType}
  locals::Vector{WType}
  body::Vector{Instruction}
end

struct Module
  funcs::Vector{Func}
end

# Printing

Base.show(io::IO, i::Const) =  print(io, i.typ, ".const ", reinterpret(jltype(i.typ), i.val))
Base.show(io::IO, i::Local) =  print(io, "get_local ", i.id)
Base.show(io::IO, i::Binary) = print(io, i.typ, ".", i.name)

function Base.show(io::IO, f::Func)
  print(io, "(func")
  foreach(p -> print(io, " (param $p)"), f.params)
  foreach(p -> print(io, " (result $p)"), f.returns)
  foreach(p -> print(io, " (local $p)"), f.locals)
  foreach(i -> print(io, "\n  $i"), f.body)
  print(io, ")")
end
