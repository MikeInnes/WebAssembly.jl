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
function getTypes(m)
  tys = [(f.params, f.returns) for f in m.funcs]
  types = collect(Set(tys))
  dict = Dict(zip(types, 0:length(types)))
  return (length(types), [(-32, length(t[1]), t[1], length(t[2]), t[2]) for t in types]), (length(tys), [[dict[t]] for t in tys]) # -32 == -0x20 :: int7
end

# Get all of the code bodies
getFunctionBodies(m, f_ids) = length(m.funcs), [addLength(vcat(toBytes((length(f.locals), [(1,l) for l in f.locals], bodyToBytes(f.body.body, f_ids))), 0x0b)) for f in m.funcs]

bodyToBytes(is, f_ids) = Vector{UInt8}(vcat([byte_op(i, f_ids) for i in is]...)) :: Vector{UInt8}

byte_op(i :: Call, f_ids)  = vcat(opcodes[Call,()], toLeb128(f_ids[i.name]))
byte_op(i :: Local, _)     = vcat(opcodes[Local,()], toLeb128(i.id))
byte_op(i :: Const, _)     = vcat(opcodes[Const, (i.typ,)], toLeb128(i.val))
byte_op(i :: Branch, _)    = vcat(opcodes[Branch, (i.cond,)], toLeb128(i.level))
byte_op(i :: SetLocal, _)  = vcat(opcodes[SetLocal, (i.tee,)], toLeb128(i.id))
byte_op(i :: If, f_ids)    = vcat(opcodes[If], types[i.result], bodyToBytes(i.t, f_ids), opcodes[:else], bodyToBytes(i.f, f_ids), opcodes[:end])
byte_op(i :: Union{Block,Loop}, f_ids) = vcat(opcodes[typeof(i)], types[i.result], bodyToBytes(i.body, f_ids), opcodes[:end])
byte_op(i :: T, _) where T <: Instruction = opcodes[i]

const external_kind =
  Dict(
    :func   => 0,
    :table  => 1,
    :memory => 2,
    :global => 3
  )

const external_kind_r =
  Dict(
    0x00 => :func,
    0x01 => :table,
    0x02 => :memory,
    0x03 => :global
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
addLength(xs :: Vector{UInt8}) = vcat(toBytes(length(xs)), xs)
# addLength(xs :: Vector{UInt8}) = vcat(toBytes(100), xs)

toBytes(xs :: Vector{UInt8}) = xs
toBytes(xs :: Union{Array, Tuple}) = isempty(xs) ? Vector{UInt8}() : vcat((map(toBytes, xs))...)
# toBytes(xs :: Tuple) = unshift!(collect(Iterators.flatten([toBytes(x) for x in xs])), toBytes(length(xs))...)
toBytes(xs :: Union{String, Symbol}) = addLength(utf8(xs))
toBytes(x :: Integer) = toLeb128(x)
toBytes(x :: WType) = types[x]
# toBytes(i :: Local) =

#### Read a bytecode file

function getByteFile(filename)
  f = open(filename, "r")
  bs = read(f)
  close(f)
  return bs
end

loadReadModule(filename) = filename |> getByteFile |> readModule

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

function readBodies(f, bodies, func_names)
  forCount(f) do
    body_size = readLeb128(f)
    read(f, body_size)
  end
end

function readBodies(f, bodies)
  readArray(f, bodies) do
    read(f, readLeb128(f))
  end
end

function bodiesToCode(f, bodies, func_names)
  forCount(f) do
    body_size = readLeb128(f)
    locals = Vector{WType}()
    forCount(f) do
      count = readLeb128(f, UInt32)
      typ = types_r[read1(f)]
      push!(locals, fill(typ, count)...)
    end
    _, body, _ = readBody(f, func_names, true)
    block = Block(body)
    push!(bodies, (locals, block))
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
  # @show name_type
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
    # @show payload_len
    if id == 0
      # Only gets function names for now
      section_end = position(f) + payload_len
      name = readutf8(f)
      if name == "name"
        while position(f) < section_end
          nameSection(f, names)
        end
      end
      # i, name_len = readLeb128(f, UInt32)
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
  return values
end

function readLeb128(f, typ=Int32)
  bs = read(f, 1)
  while (bs[end] & 0x80 != 0)
    push(bs, read1(f))
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
