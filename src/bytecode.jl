# Converts a module to its bytecode representation.

# The bytecode layout is somewhat different to the structure of the wasm IR. It
# therefore makes sense to restructure the IR into an intermediary format and
# then make a short hop to bytecode. The length of each section needs to be
# saved alongside it which would require a second pass anyway.

const magic   = 0x6d736100 # \0asm
const version = 0x00000001 # Targeting version 1
const preamble = reinterpret(UInt8, [magic, version])

# Converts any Integer type to Leb128 format as an array of bytes.
function toLeb128(x :: Integer)
  len = sizeof(x) * 8
  bytes = Vector{UInt8}()

  # do while.
  byte = UInt8(x & 0x7F) | 0x80 # Set the continuation bit.
  x >>= 7
  push!(bytes, byte)
  while !(x == 0 && (byte & 0x40 == 0)) && !(x == -1 && (byte & 0x40 != 0))
    byte = UInt8(x & 0x7F) | 0x80 # Set the continuation bit.
    x >>= 7
    push!(bytes, byte)
  end
  bytes[end] &= 0x7F # Unset the final continuation bit.
  return bytes
end

# Converts array of bytes to Integer type. Assumes the array is of only the
# relevant bytes and thus ignores continuation bits.
function fromLeb128(bs, typ=BigInt)
  result = typ(0)
  shift = 0
  for i in eachindex(bs)
    result |= typ(bs[i] & 0x7F) << shift
    shift += 7
  end
  if bs[end] & 0x40 != 0
    result |= (typ(-1) << shift)
  end
  return result
end

# Get the raw utf8 bytes of a string.
utf8(x :: String) = Vector{UInt8}(x)
utf8(x) = x |> string |> utf8

# Take a module and return an Array of function types and an Array of the types
# that each function uses.

# In the binary representation each function is allowed multiple types, as all
# types are being added to the type section they will each only be given one.
function getTypes(m)
  tys = [(f.params, f.returns) for f in m.funcs]
  types = collect(Set(tys))
  dict = Dict(zip(types, 0:length(types)))
  return (length(types), [(-32, length(t[1]), t[1], length(t[2]), t[2]) for t in types]), (length(tys), [[dict[t]] for t in tys]) # -32 == -0x20 :: int7
end

# Get all of the code bodies
getFunctionBodies(m, f_ids) = length(m.funcs), [addLength(vcat(toBytes((length(f.locals), [(1,l) for l in f.locals], bodyToBytes(f.body.body, f_ids))), 0x0b)) for f in m.funcs]

bodyToBytes(is, f_ids) = Vector{UInt8}(vcat([byte_op(i, f_ids) for i in is]...)) :: Vector{UInt8}
byte_op(i :: Local, f_ids) = UInt8[0x20, i.id]
byte_op(i :: SetLocal, f_ids) = UInt8[i.tee ? 0x22 : 0x21, i.id]
byte_op(i :: Const, f_ids) = UInt8[opcodes[i.typ][:const], toLeb128(i.val)...]
byte_op(i :: Op, f_ids) = opcodes[i.typ][i.name] :: UInt8
byte_op(i :: If, f_ids) = vcat(0x04, 0x40, bodyToBytes(i.t, f_ids), 0x05, bodyToBytes(i.f, f_ids), 0x0b) :: Vector{UInt8}
byte_op(i :: Block, f_ids) = vcat(0x02, 0x40, bodyToBytes(i.body, f_ids), 0x0b) :: Vector{UInt8}
byte_op(i :: Loop, f_ids) = vcat(0x03, 0x40, bodyToBytes(i.body, f_ids), 0x0b) :: Vector{UInt8}
byte_op(i :: Branch, f_ids) = vcat(i.cond ? 0x0d : 0x0c, toLeb128(i.level)) :: Vector{UInt8}
byte_op(i :: Return, f_ids) = 0x0f
byte_op(i :: Select, f_ids) = 0x1b
byte_op(i :: Unreachable, f_ids) = 0x00
byte_op(i :: Nop, f_ids) = 0x01
byte_op(i :: Call, f_ids) = vcat(0x10, toLeb128(f_ids[i.name])) :: Vector{UInt8}

# byte_op(i) = @show i


opcodes_i32 =
  Dict(
    :const  =>  0x41,
    :eqz	  =>  0x45,
    :eq	    =>  0x46,
    :ne	    =>  0x47,
    :lt_s	  =>  0x48,
    :lt_u	  =>  0x49,
    :gt_s	  =>  0x4a,
    :gt_u	  =>  0x4b,
    :le_s	  =>  0x4c,
    :le_u	  =>  0x4d,
    :ge_s	  =>  0x4e,
    :ge_u	  =>  0x4f,
    :clz    =>  0x67,
    :ctz    =>  0x68,
    :popcnt =>  0x69,
    :add    =>  0x6a,
    :sub    =>  0x6b,
    :mul    =>  0x6c,
    :div_s  =>  0x6d,
    :div_u  =>  0x6e,
    :rem_s  =>  0x6f,
    :rem_u  =>  0x70,
    :and    =>  0x71,
    :or     =>  0x72,
    :xor    =>  0x73,
    :shl    =>  0x74,
    :shr_s  =>  0x75,
    :shr_u  =>  0x76,
    :rotl   =>  0x77,
    :rotr   =>  0x78
  )

opcodes_i64 =
  Dict(
    :const  =>	0x42,
    :eqz	  =>  0x50,
    :eq	    =>  0x51,
    :ne	    =>  0x52,
    :lt_s	  =>  0x53,
    :lt_u	  =>  0x54,
    :gt_s	  =>  0x55,
    :gt_u	  =>  0x56,
    :le_s	  =>  0x57,
    :le_u	  =>  0x58,
    :ge_s	  =>  0x59,
    :ge_u	  =>  0x5a,
    :clz    =>	0x79,
    :ctz    =>	0x7a,
    :popcnt =>	0x7b,
    :add    =>	0x7c,
    :sub    =>	0x7d,
    :mul    =>	0x7e,
    :div_s  =>	0x7f,
    :div_u  =>	0x80,
    :rem_s  =>	0x81,
    :rem_u  =>	0x82,
    :and    =>	0x83,
    :or     =>	0x84,
    :xor    =>	0x85,
    :shl    =>	0x86,
    :shr_s  =>	0x87,
    :shr_u  =>	0x88,
    :rotl   =>	0x89,
    :rotr   =>	0x8a
  )

opcodes_f32 =
  Dict(
    :const    =>  0x43,
    :eq       =>  0x5b,
    :ne       =>  0x5c,
    :lt       =>  0x5d,
    :gt       =>  0x5e,
    :le       =>  0x5f,
    :ge       =>  0x60,
    :abs      =>  0x8b,
    :neg      =>  0x8c,
    :ceil     =>  0x8d,
    :floor    =>  0x8e,
    :trunc    =>  0x8f,
    :nearest  =>  0x90,
    :sqrt     =>  0x91,
    :add      =>  0x92,
    :sub      =>  0x93,
    :mul      =>  0x94,
    :div      =>  0x95,
    :min      =>  0x96,
    :max      =>  0x97,
    :copysign =>  0x98
  )

opcodes_f64 =
  Dict(
    :const    =>  0x44,
    :eq       =>  0x61,
    :ne       =>  0x62,
    :lt       =>  0x63,
    :gt       =>  0x64,
    :le       =>  0x65,
    :ge       =>  0x66,
    :abs      =>  0x99,
    :neg      =>  0x9a,
    :ceil     =>  0x9b,
    :floor    =>  0x9c,
    :trunc    =>  0x9d,
    :nearest  =>  0x9e,
    :sqrt     =>  0x9f,
    :add      =>  0xa0,
    :sub      =>  0xa1,
    :mul      =>  0xa2,
    :div      =>  0xa3,
    :min      =>  0xa4,
    :max      =>  0xa5,
    :copysign =>  0xa6
  )

opcodes =
  Dict(
    i32 => opcodes_i32,
    i64 => opcodes_i64,
    f32 => opcodes_f32,
    f64 => opcodes_f64
  )

const external_kind =
  Dict(
    :func   => 0,
    :table  => 1,
    :memory => 2,
    :global => 3
  )

# Construct dictionaries from names to index for each index space, and then
# use them to construct the exports.

# Currently just memory and functions.
function getExports(m, space)
  return length(m.exports), [(e.name, external_kind[e.typ], space[e.typ][e.internalname]) for e in m.exports]
end

function getModule(m)
  f_ids = Dict(zip([f.name for f in m.funcs], 0:length(m.funcs)))
  m_ids  = Dict(zip([mem.name for mem in m.mems], 0:length(m.mems)))
  space = Dict(:memory => m_ids, :func => f_ids)

  types, funcs = getTypes(m)
  exports = getExports(m, space)
  code = getFunctionBodies(m, f_ids)
  # types_, funcs_, exports_, code_ = map(toBytes, (types, funcs, exports, code))
  # sections = vcat([vcat(toBytes(s[1]),addLength(toBytes(s[2]))) for s in [(1, types), (3, funcs), (7, exports), (10, code)]]...)
  sections = vcat([vcat(toBytes(s[1]),addLength(toBytes(s[2]))) for s in [(1, types), (3, funcs), (7, exports), (10, code)]]...)
  # sections = toBytes([(1, types), (3, funcs), (7, exports)])

  return vcat(preamble, sections)
end

# Currently assuming the arrays always have the form length of array followed by
# the array, same for strings.
addLength(xs :: Vector{UInt8}) = vcat(toBytes(length(xs)), xs) :: Vector{UInt8}

toBytes(xs :: Vector{UInt8}) = xs
toBytes(xs :: Union{Array, Tuple}) = Vector{UInt8}(vcat(map(toBytes, xs)...))
# toBytes(xs :: Tuple) = unshift!(collect(Iterators.flatten([toBytes(x) for x in xs])), toBytes(length(xs))...)
toBytes(xs :: Union{String, Symbol}) = addLength(utf8(xs)) :: Vector{UInt8}
toBytes(x :: Integer) = toLeb128(x) :: Vector{UInt8}
toBytes(x :: WType) = types[x] :: UInt8
# toBytes(i :: Local) =

types =
  Dict(
    i32 => 0x7f,
    i64 => 0x7e,
    f32 => 0x7d,
    f64 => 0x7c
  )
