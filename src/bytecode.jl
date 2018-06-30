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
  if !(typ <: Unsigned) && bs[end] & 0x40 != 0
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
function getTypes(fs)
  tys = [(f.params, f.returns) for f in fs]
  ts = collect(Set(tys))
  dict = Dict(zip(ts, 0:length(ts)))
  return (length(ts), [(Lookup(:func, types), length(t[1]), t[1], length(t[2]), t[2]) for t in ts]), (length(tys), [[dict[t]] for t in tys])
end

# A tag meaning the size of the following data in bytes should be stored.
struct Length
  x
end

# Defers a lookup in a dictionary until conversion to bytes
struct Lookup
  x :: Any
  dict :: Dict{Any, Any}
end

getFunctionBodies(fs) = length(fs), [Length((length(f.locals), [(1,l) for l in f.locals], f.body.body, Lookup(:end, opcodes))) for f in fs]

function getExports(es, space)
  return length(es), [(e.name, Lookup(e.typ, external_kind), space[e.typ][e.internalname]) for e in es]
end

function getModule(m)
  f_ids = Dict(zip([f.name for f in m.funcs], 0:length(m.funcs)))
  m_ids  = Dict(zip([mem.name for mem in m.mems], 0:length(m.mems)))
  space = Dict(:memory => m_ids, :func => f_ids)

  types, funcs = getTypes(m.funcs)
  exports = getExports(m.exports, space)
  code = getFunctionBodies(m.funcs)

  # Pair individual sections in order with their index
  sections = [(1, types), (3, funcs), (7, exports), (10, code)]

  # Add length parameter and convert to bytes
  bytes = toBytes([(n, Length(s)) for (n, s) in sections], f_ids)

  return vcat(preamble, bytes)
end

addLength(xs :: Vector{UInt8}) = vcat(xs |> length |> UInt32 |> toLeb128, xs)

toBytes(x  :: UInt8, _) = error("There should be no compiled code before toBytes() is called.")
toBytes(l  :: Length, f_ids) = addLength(toBytes(l.x, f_ids))
toBytes(xs :: Union{Array, Tuple}, f_ids) = isempty(xs) ? Vector{UInt8}() : vcat((map(x -> toBytes(x, f_ids), xs))...)
toBytes(xs :: Union{String, Symbol}, _) = addLength(utf8(xs))
toBytes(x  :: Integer, _) = toLeb128(x)
toBytes(x  :: WType, _) = types[x]

toBytes(is :: Vector{Instruction}, f_ids) = Vector{UInt8}(vcat([toBytes(i, f_ids) for i in is]...)) :: Vector{UInt8}
toBytes(i  :: Call, f_ids)  = vcat(opcodes[Call,()], toLeb128(f_ids[i.name]))
toBytes(i  :: Local, _)     = vcat(opcodes[Local,()], toLeb128(i.id))
toBytes(i  :: Const, _)     = vcat(opcodes[Const, (i.typ,)], toLeb128(i.val))
toBytes(i  :: Branch, _)    = vcat(opcodes[Branch, (i.cond,)], toLeb128(i.level))
toBytes(i  :: SetLocal, _)  = vcat(opcodes[SetLocal, (i.tee,)], toLeb128(i.id))
toBytes(i  :: If, f_ids)    = vcat(opcodes[If], types[i.result], toBytes(i.t, f_ids), opcodes[:else], toBytes(i.f, f_ids), opcodes[:end])
toBytes(i  :: Union{Block,Loop}, f_ids) = vcat(opcodes[typeof(i)], types[i.result], toBytes(i.body, f_ids), opcodes[:end])
toBytes(i  :: Instruction, _) = opcodes[i]
toBytes(i  :: Lookup, _) = i.dict[i.x]

function readTypes(f, types)
  readArray(f, types) do
    form = readLeb128(f, Int8)
    form == -32 || error("Not a valid function")
    params = getRegisters(f)
    returns = getRegisters(f)
    return (params, returns)
  end
end

function readMemory(f, memory)
  readArray(f, memory) do
    flag = readLeb128(f, UInt8)
    initial = readLeb128(f, UInt32)
    maximum = Void()
    if flag == 0x01
      maximum = readLeb128(i,bs,UInt32)
    end
    return (initial, maximum)
  end
end

read1(f) = read(f, 1)[1]

function readExports(f, exports)
  readArray(f, exports) do
    name = readsymbol(f)
    kind = external_kind_r[read1(f)]
    index = readLeb128(f, UInt32)
    return (name, kind, index)
  end
end

function bodiesToCode(f, bodies, func_names)
  readArray(f, bodies) do
    body_size = readLeb128(f)
    locals = Vector{WType}()
    forCount(f) do
      count = readLeb128(f, UInt32)
      typ = types_r[read1(f)]
      push!(locals, fill(typ, count)...)
    end
    _, body, _ = readBody(f, func_names, true)
    return (locals, Block(body))
  end
end

function readFuncTypes(f, func_types)
  getNumArray(f, UInt32, func_types)
end

function readNameMap(f, names)
  forCount(f) do
    index = readLeb128(f)
    name = readsymbol(f)
    names[index] = name
  end
end

function nameSection(f, names)
  name_type = readLeb128(f, UInt8)
  name_payload_len = readLeb128(f, UInt8)
  name_type == 1 || return skip(f, name_payload_len)
  readNameMap(f, names[:func])
end

function readModule(f)
  if read(f, length(preamble)) != preamble
    error("Something wrong with preamble. Version 1 only.")
  end
  i = length(preamble) + 1
  id = id_ = -1

  types = Vector{Tuple{Vector{WType}, Vector{WType}}}()
  func_types = Vector{UInt32}()
  exports = Vector{Tuple{Symbol, Symbol, Int}}()
  memory = Vector{Tuple{UInt32, Union{UInt32, Void}}}()
  bodies = Vector{Tuple{Vector{WType}, Block}}()
  bodies_f = 0

  # A dictionary from int to name for each index space.
  d = Dict{UInt32, Symbol}
  u = Union{Vector{Symbol}, d}
  names = Dict{Symbol, u}(:func => d(), :memory => d())

  mem_names = Vector{Symbol}()
  while !eof(f)
    id_ = id
    id = readLeb128(f, UInt8)
    id > id_ || id == 0 || error("Sections must be in increasing order.")
    payload_len = readLeb128(f, UInt32)
    if id == 0
      # Only gets function names for now
      section_end = position(f) + payload_len
      name = readutf8(f)
      if name == "name"
        while position(f) < section_end
          nameSection(f, names)
        end
      end
    elseif id == 1 # Types
      readTypes(f, types)
    elseif id == 3 # Functions
      readFuncTypes(f, func_types)
    elseif id == 5 # Memory
      readMemory(f, memory)
    elseif id == 7 # Exports
      readExports(f, exports)
    elseif id == 10 # Bodies, do this later when names are sorted
      bodies_f = position(f)
      skip(f, payload_len)
    else
      error("Unknown Section")
    end
  end
  # id == 10 || error("No code in file")
  getNames(names, [(:func, length(func_types)), (:memory, length(memory))])
  seek(f, bodies_f)
  bodiesToCode(f, bodies, names[:func])
  length(bodies) == length(func_types) == length(names[:func]) || error("Number of function types and function bodies does not match.")
  funcs = [Func(n, types[t+1]..., b...) for (n, t, b) in zip(names[:func], func_types, bodies)]
  mems  = [Mem(n, m...) for (n, m) in zip(names[:memory], memory)]
  exports = [Export(n, names[is][i+1], is) for (n, is, i) in exports]

  return Module([], funcs, [], mems, [], [], [], Ref(0), [], exports)
end

function getNames(names, spacelength)
  for (s, l) in spacelength
    push!(names, s => [get(names[s], i, Symbol(s,"_$i")) for i in (0:l-1)])
  end
end

getRegisters(f) = [types_r[b] for b in getBytes(f)]

function getNumArray(f, typ, values)
  readArray(f, values) do
    readLeb128(f, typ)
  end
end

function readLeb128(f, typ=Int32)
  bs = read(f, 1)
  while (bs[end] & 0x80 != 0)
    push!(bs, read1(f))
  end
  return fromLeb128(bs, typ)
end

getBytes(f) = read(f, readLeb128(f, UInt32))
readutf8(f) = f |> getBytes |> String
readsymbol(f) = f |> getBytes |> Symbol

function readBody(f, fns, else_=false)
  is = Vector{Instruction}()
  result = else_ ? Void() : types_r[read1(f)]
  b = read1(f)
  while (b != opcodes[:end]) && (b != opcodes[:else])
    op = readOp(opcodes_r[b], f, fns) :: Instruction
    push!(is, op)
    b = read1(f)
  end
  return b, is, result
end

function readOp(block :: DataType, f, fns)
  b, is, r = readBody(f, fns)
  if block == If
    f_is = Vector{Instruction}()
    if b == opcodes[:else]
      _, f_is, _ = readBody(f, fns, true)
    end
    return If(is, f_is, r)
  else
    return block(is, r)
  end
end

readOp(x::WebAssembly.Instruction, f, fns) = x

function readOp(x :: Tuple{DataType, T}, f, fns) where T
  x[1] == Const && return Const(readLeb128(f, jltype(x[2][1])))
  arg = readLeb128(f)
  x[1] == Call && return Call(fns[arg + 1])
  return x[1](x[2]...,arg)
end

# Given a function for reading an item, read an array of those items.
# Assumes that the next item to be read is a UInt32 of the length of the array
# followed by the array itself.
function readArray(f, fd, result=Vector{Any}())
  forCount(fd) do
    val = f()
    push!(result, val)
  end
  return result
end

# Repeat an operation the number of times given by the read in count.
function forCount(f, fd)
  count = readLeb128(fd, UInt32)
  for j in 1:count
    f()
  end
end

const types =
  Dict(
    i32 => 0x7f,
    i64 => 0x7e,
    f32 => 0x7d,
    f64 => 0x7c,
    :anyfunc => 0x70,
    :func => 0x60,
    Void() => 0x40
  )

const types_r = map(reverse, types)

const opcodes =
  Dict(

    Return()      => 0x0f,
    Select()      => 0x1b,
    Unreachable() => 0x00,
    Nop()         => 0x01,

    (Local,()) => 0x20,

    (SetLocal, (true,))  => 0x22,
    (SetLocal, (false,)) => 0x21,

    (Const, (i32,))  =>  0x41,
    (Const, (i64,))  =>	0x42,
    (Const, (f32,))  =>  0x43,
    (Const, (f64,))  =>  0x44,

    (Branch, (true,))  => 0x0d,
    (Branch, (false,)) => 0x0c,

    (Call,()) => 0x10,

    If    => 0x04,
    :else    => 0x05,
    :end     => 0x0b,
    Block => 0x02,
    Loop  => 0x03,

    Op(i32, :eqz)	   =>  0x45,
    Op(i32, :eq)	   =>  0x46,
    Op(i32, :ne)	   =>  0x47,
    Op(i32, :lt_s)	 =>  0x48,
    Op(i32, :lt_u)	 =>  0x49,
    Op(i32, :gt_s)	 =>  0x4a,
    Op(i32, :gt_u)	 =>  0x4b,
    Op(i32, :le_s)	 =>  0x4c,
    Op(i32, :le_u)	 =>  0x4d,
    Op(i32, :ge_s)	 =>  0x4e,
    Op(i32, :ge_u)	 =>  0x4f,
    Op(i32, :clz)    =>  0x67,
    Op(i32, :ctz)    =>  0x68,
    Op(i32, :popcnt) =>  0x69,
    Op(i32, :add)    =>  0x6a,
    Op(i32, :sub)    =>  0x6b,
    Op(i32, :mul)    =>  0x6c,
    Op(i32, :div_s)  =>  0x6d,
    Op(i32, :div_u)  =>  0x6e,
    Op(i32, :rem_s)  =>  0x6f,
    Op(i32, :rem_u)  =>  0x70,
    Op(i32, :and)    =>  0x71,
    Op(i32, :or)     =>  0x72,
    Op(i32, :xor)    =>  0x73,
    Op(i32, :shl)    =>  0x74,
    Op(i32, :shr_s)  =>  0x75,
    Op(i32, :shr_u)  =>  0x76,
    Op(i32, :rotl)   =>  0x77,
    Op(i32, :rotr)   =>  0x78,

    Op(i64, :eqz)	   =>  0x50,
    Op(i64, :eq)	   =>  0x51,
    Op(i64, :ne)	   =>  0x52,
    Op(i64, :lt_s)	 =>  0x53,
    Op(i64, :lt_u)	 =>  0x54,
    Op(i64, :gt_s)	 =>  0x55,
    Op(i64, :gt_u)	 =>  0x56,
    Op(i64, :le_s)	 =>  0x57,
    Op(i64, :le_u)	 =>  0x58,
    Op(i64, :ge_s)	 =>  0x59,
    Op(i64, :ge_u)	 =>  0x5a,
    Op(i64, :clz)    =>	 0x79,
    Op(i64, :ctz)    =>	 0x7a,
    Op(i64, :popcnt) =>	 0x7b,
    Op(i64, :add)    =>	 0x7c,
    Op(i64, :sub)    =>	 0x7d,
    Op(i64, :mul)    =>	 0x7e,
    Op(i64, :div_s)  =>	 0x7f,
    Op(i64, :div_u)  =>	 0x80,
    Op(i64, :rem_s)  =>	 0x81,
    Op(i64, :rem_u)  =>	 0x82,
    Op(i64, :and)    =>	 0x83,
    Op(i64, :or)     =>	 0x84,
    Op(i64, :xor)    =>	 0x85,
    Op(i64, :shl)    =>	 0x86,
    Op(i64, :shr_s)  =>	 0x87,
    Op(i64, :shr_u)  =>	 0x88,
    Op(i64, :rotl)   =>	 0x89,
    Op(i64, :rotr)   =>	 0x8a,

    Op(f32, :eq)       =>  0x5b,
    Op(f32, :ne)       =>  0x5c,
    Op(f32, :lt)       =>  0x5d,
    Op(f32, :gt)       =>  0x5e,
    Op(f32, :le)       =>  0x5f,
    Op(f32, :ge)       =>  0x60,
    Op(f32, :abs)      =>  0x8b,
    Op(f32, :neg)      =>  0x8c,
    Op(f32, :ceil)     =>  0x8d,
    Op(f32, :floor)    =>  0x8e,
    Op(f32, :trunc)    =>  0x8f,
    Op(f32, :nearest)  =>  0x90,
    Op(f32, :sqrt)     =>  0x91,
    Op(f32, :add)      =>  0x92,
    Op(f32, :sub)      =>  0x93,
    Op(f32, :mul)      =>  0x94,
    Op(f32, :div)      =>  0x95,
    Op(f32, :min)      =>  0x96,
    Op(f32, :max)      =>  0x97,
    Op(f32, :copysign) =>  0x98,

    Op(f64, :eq)       =>  0x61,
    Op(f64, :ne)       =>  0x62,
    Op(f64, :lt)       =>  0x63,
    Op(f64, :gt)       =>  0x64,
    Op(f64, :le)       =>  0x65,
    Op(f64, :ge)       =>  0x66,
    Op(f64, :abs)      =>  0x99,
    Op(f64, :neg)      =>  0x9a,
    Op(f64, :ceil)     =>  0x9b,
    Op(f64, :floor)    =>  0x9c,
    Op(f64, :trunc)    =>  0x9d,
    Op(f64, :nearest)  =>  0x9e,
    Op(f64, :sqrt)     =>  0x9f,
    Op(f64, :add)      =>  0xa0,
    Op(f64, :sub)      =>  0xa1,
    Op(f64, :mul)      =>  0xa2,
    Op(f64, :div)      =>  0xa3,
    Op(f64, :min)      =>  0xa4,
    Op(f64, :max)      =>  0xa5,
    Op(f64, :copysign) =>  0xa6,

    Convert(i32, i64, :wrap)         =>	0xa7,
    Convert(i32, f32, :trunc_s)      =>	0xa8,
    Convert(i32, f32, :trunc_u)      =>	0xa9,
    Convert(i32, f64, :trunc_s)      =>	0xaa,
    Convert(i32, f64, :trunc_u)      =>	0xab,
    Convert(i64, i32, :extend_s)     =>	0xac,
    Convert(i64, i32, :extend_u)     =>	0xad,
    Convert(i64, f32, :trunc_s)      =>	0xae,
    Convert(i64, f32, :trunc_u)      =>	0xaf,
    Convert(i64, f64, :trunc_s)      =>	0xb0,
    Convert(i64, f64, :trunc_u)      =>	0xb1,
    Convert(f32, i32, :convert_s)    =>	0xb2,
    Convert(f32, i32, :convert_u)    =>	0xb3,
    Convert(f32, i64, :convert_s)    =>	0xb4,
    Convert(f32, i64, :convert_u)    =>	0xb5,
    Convert(f32, f64, :demote)       =>	0xb6,
    Convert(f64, i32, :convert_s)    =>	0xb7,
    Convert(f64, i32, :convert_u)    =>	0xb8,
    Convert(f64, i64, :convert_s)    =>	0xb9,
    Convert(f64, i64, :convert_u)    =>	0xba,
    Convert(f64, f32, :promote)      =>	0xbb,
    Convert(i32, f32, :reinterpret)  =>	0xbc,
    Convert(i64, f64, :reinterpret)  =>	0xbd,
    Convert(f32, i32, :reinterpret)  =>	0xbe,
    Convert(f64, i64, :reinterpret)  =>	0xbf
  )

# Reverse dictionary of all opcodes including conversions.
const opcodes_r = map(reverse, opcodes)

const external_kind =
  Dict(
    :func   => 0x00,
    :table  => 0x01,
    :memory => 0x02,
    :global => 0x03
  )

const external_kind_r = map(reverse, external_kind)
