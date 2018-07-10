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
  result = zero(typ)
  shift = 0
  for i in eachindex(bs)
    result |= typ(bs[i] & 0x7F) << shift
    shift += 7
  end
  if !(typ <: Unsigned) && bs[end] & 0x40 != 0
    result |= (typ(-1) << shift)
  end
  return typ(result)
end

###############################################################################
################               Writing Bytecode                ################
###############################################################################

# Get the raw utf8 bytes of a string.
utf8(x :: String) = Vector{UInt8}(x)
utf8(x) = x |> string |> utf8

# Take a module and return an Array of function types and an Array of the types
# that each function uses.

# In the binary representation each function is allowed multiple types, as all
# types are being added to the type section they will each only be given one.
function getTypes(fs, ifs)
  f_tys = [(f.params, f.returns) for f in fs]
  if_tys = [(f.x.params, f.x.returns) for f in ifs]
  ts = collect(Set(vcat(if_tys, f_tys)))
  dict = Dict(zip(ts, 0:length(ts)))
  return (length(ts), [(Lookup(:func, types), length(t[1]), t[1], length(t[2]), t[2]) for t in ts]), (length(fs), [[dict[t]] for t in f_tys]), Dict(i.name => dict[t] for (t, i) in zip(if_tys, ifs))
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

# Wrapper for Data during compilation
struct Bytes
  bytes :: Vector{UInt8}
end

getFunctionBodies(fs, f_ids) = length(fs), [Length((length(f.locals), [(1,l) for l in f.locals], getOps(f.body.body, f_ids), Lookup(:end, opcodes))) for f in fs]

getOps(i :: Call, f_ids)    = Lookup((Call, ()),           opcodes), f_ids[i.name]
getOps(i :: Local, _)       = Lookup((Local, ()),          opcodes), i.id
getOps(i :: Const, _)       = Lookup((Const, (i.typ,)),    opcodes), value(i)
getOps(i :: Branch, _)      = Lookup((Branch, (i.cond,)),  opcodes), i.level
getOps(i :: SetLocal, _)    = Lookup((SetLocal, (i.tee,)), opcodes), i.id
getOps(i :: Instruction, _) = Lookup(i, opcodes)
getOps(i :: GetGlobal, _)   = Lookup((GetGlobal, ()),      opcodes), i.id
getOps(i :: SetGlobal, _)   = Lookup((SetGlobal, ()),      opcodes), i.id
getOps(i :: MemoryOp, _)    = Lookup((MemoryOp, (i.op,)),  opcodes), Int(log2(i.alignment)), i.offset

getOps(is :: Vector{Instruction}, f_ids) = map(i->getOps(i, f_ids), is)
getOps(i  :: Union{Block,Loop}, f_ids) = Lookup(typeof(i), opcodes), Lookup(i.result, types), getOps(i.body, f_ids), Lookup(:end, opcodes)

function getOps(i :: If, f_ids)
  t = Lookup(If, opcodes), Lookup(i.result, types), getOps(i.t, f_ids)
  isempty(i.f) && return t, Lookup(:end, opcodes)
  return t, Lookup(:else, opcodes), getOps(i.f, f_ids), Lookup(:end, opcodes)
end

function getExports(es, space)
  return length(es), [(e.name, Lookup(e.typ, external_kind), space[e.typ][e.internalname]) for e in es]
end

# Can add a Module name and names for locals.
# Names for locals can't be made more complex than distinguishing parameters
# and locals, as any complex register allocation will break original variable
# names. Ultimately they are unneeded.
# A module name might be useful.
nameSection(fnames) = "name", 1, Length((length(fnames), zip(0:length(fnames)-1, fnames) |> collect))

globalSection(gs) = length(gs), [(Lookup(g.typ, types), g.mut, getOps(Const(jltype(g.typ)(g.init)), []), Lookup(:end, opcodes)) for g in gs]

get_init_expr(g::Global) = getOps(Const(jltype(g.typ)(g.init)), []), Lookup(:end, opcodes)
get_init_expr(d::Data)   = getOps(Const(d.offset), []), Lookup(:end, opcodes)

function startSection(start::Symbol, fs, is)
  println("here")
  start_index = 0
  for f in is
    f.typ == :func || continue
    start == f.name && return start_index
    start_index = start_index + 1
  end
  for f in fs
    start == f.name && return start_index
    start_index = start_index + 1
  end
  return start_index
end

startSection(start, _) = nothing

function memorySection(mems)
  length(mems), [m.max == nothing ? (false, m.min) : (true, m.min, max) for m in mems]
end

function importSection(imports, types)
  length(imports), [(i.mod, i.field_str, Lookup(i.typ, external_kind), i.typ == :func ? types[i.name] : error("Only func supported in imports.")) for i in imports]
end

function dataSection(data)
  length(data), [(d.memidx, get_init_expr(d), Length(Bytes(d.data))) for d in data]
end

# Reads in base.wasm, merges it with the module m and the calls getModule.
mergeWithBase(m) = merge_module(getBase(), m)

getBase() = readModule("src/base.wasm")

# Assumes only one module has globals, data sections don't overlap e.t.c.
# Not very general.
function merge_module(m,n)
  # Take the biggest minimum, assume no maximum
  mem = m.mems[1].min > n.mems[1].min ? Mem(m.mems[1].name, m.mems[1].min, nothing) : Mem(n.mems[1].name, n.mems[1].min, nothing)
  globals = isempty(m.globals) ? n.globals : m.globals
  start = m.start == nothing ? n.start : m.start

  return Module([], vcat(m.funcs, n.funcs), [], [mem], globals, [], vcat(m.data, n.data), start, vcat(m.imports, n.imports), vcat(n.exports, m.exports))
end


function getModule(m)
  f_imp = Iterators.filter(x -> x.typ == :func, m.imports) |> collect
  fnames = vcat([i.name for i in f_imp],[f.name for f in m.funcs])
  f_ids = Dict(zip(fnames, 0:length(m.funcs)+length(f_imp)))
  m_ids  = Dict(zip([mem.name for mem in m.mems], 0:length(m.mems)))
  space = Dict(:memory => m_ids, :func => f_ids)
  types, funcs, if_types = getTypes(m.funcs, f_imp)
  exports = getExports(m.exports, space)
  code = getFunctionBodies(m.funcs, f_ids)
  names = nameSection(fnames)
  globals = globalSection(m.globals)
  # globals = 0
  # @show m.mems
  memory = memorySection(m.mems)
  println("what")
  start = m.start == nothing ? nothing : f_ids[m.start]
  # @show start
  imports = importSection(m.imports, if_types)
  # @show imports
  data = dataSection(m.data)
  # data = 0
  # @show data
  # @show globals
  #

  @show names
  # # Pair individual sections in order with their index
  @show types
  sections = [ (1, types)
             , (2, imports)
             , (3, funcs)
             , (5, memory)
             , (6, globals)
             , (7, exports)
             , (8, start)
             , (10, code)
             , (11, data)
             , (0, names)
             ]

  filter!(e->e[2] != nothing, sections)

  # Add length parameter and convert to bytes
  bytes = toBytes([(n, Length(s)) for (n, s) in sections])

  return vcat(preamble, bytes)
end

addLength(xs :: Vector{UInt8}) = vcat(xs |> length |> UInt32 |> toLeb128, xs)

toBytes(x  :: UInt8) = error("There should be no compiled code before toBytes() is called.")
toBytes(l  :: Length) = addLength(toBytes(l.x))
toBytes(xs :: Union{Array, Tuple}) = isempty(xs) ? Vector{UInt8}() : vcat(map(toBytes, xs)...)
toBytes(s  :: Union{String, Symbol}) = addLength(utf8(s))
toBytes(x  :: Integer) = toLeb128(x)
toBytes(x  :: WType) = types[x]
toBytes(i  :: Lookup) = i.dict[i.x]
toBytes(i  :: Bytes)   = i.bytes

###############################################################################
################               Reading Bytecode                ################
###############################################################################

function readTypes(f)
  readArray(f, Vector{Tuple{Vector{WType}, Vector{WType}}}()) do
    form = readLeb128(f, Int8)
    form == -32 || error("Not a valid function")
    params = getRegisters(f)
    returns = getRegisters(f)
    return (params, returns)
  end
end

function readMemory(f)
  readArray(f, Vector{Tuple{UInt32, Union{UInt32, Void}}}()) do
    flag = readLeb128(f, Bool)
    initial = readLeb128(f, UInt32)
    maximum = flag ? readLeb128(i,bs,UInt32) : Void()
    return (initial, maximum)
  end
end

read1(f) = read(f, 1)[1]

function readExports(f)
  readArray(f, Vector{Tuple{Symbol, Symbol, Int}}()) do
    name = readsymbol(f)
    kind = external_kind_r[read1(f)]
    index = readLeb128(f, UInt32)
    return (name, kind, index)
  end
end

function bodiesToCode(f, func_names)
  readArray(f, Vector{Tuple{Vector{WType}, Block}}()) do
    body_size = readLeb128(f)
    locals = Vector{WType}()
    forCount(f) do
      count = readLeb128(f, UInt32)
      typ = types_r[read1(f)]
      push!(locals, fill(typ, count)...)
    end
    return (locals, Block(readBody(f, func_names, true)[1]))
  end
end

readFuncTypes(f) = getNumArray(f, UInt32)

function readNameMap(f, names)
  forCount(f) do
    index = readLeb128(f)
    name = readsymbol(f)
    names[index] = name
  end
end

function readNames(f, names)
  name_type = readLeb128(f, UInt8)
  name_payload_len = readLeb128(f, UInt8)
  name_type == 1 || return (skip(f, name_payload_len); false)
  readNameMap(f, names[:func])
  return true
end

function readGlobals(f)
  readArray(f, Vector{Global}()) do
    typ = types_r[read1(f)]
    mut = Bool(read1(f))
    init = init_expr(f)
    return Global(typ, mut, init)
  end
end

# Init exprs need to be interpreted at compile time to get the value. Currently
# only a very limited set is supported, so it serves to just run the interpreter
# on this code without the context that would normally be needed.

# TODO: Globals can be set in terms of other (immutable) globals.
function init_expr(f)
  body = readBody(f, [], true)[1]
  ms = Vector()
  runBody(body,ms,0,[])
  return ms[1]
end

function readData(f)
  readArray(f, Vector{Data}()) do
    index = readLeb128(f, UInt32)
    offset = init_expr(f)
    data = readBytes(f)
    return Data(index, offset, data)
  end
end

function readImports(f)
  readArray(f, Vector()) do
    mod = readsymbol(f)
    name = readsymbol(f)
    kind = external_kind_r[read1(f)]
    kind == :func || error("Other import kinds not yet supported")
    typ = readLeb128(f, UInt32)
    return mod, name, kind, typ
  end
end

readModule(filename::String) = open(f -> readModule(f), filename, "r")

function readModule(f)
  if read(f, length(preamble)) != preamble
    error("Something wrong with preamble. Version 1 only.")
  end
  id = id_ = -1

  types = f_types = exports = memory = globals = data = imports = Vector()
  bodies_f = -1

  # A dictionary from int to name for each index space.
  d = Dict{UInt32, Symbol}
  names = Dict(:func => d(), :memory => d())
  start_index = -1

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
        read_func_names = false
        while !read_func_names && position(f) < section_end
          read_func_names = readNames(f, names)
        end
      end
      seek(f, section_end)
    elseif id == 1 # Types
      types = readTypes(f)
    elseif id == 2 # Ignore imports for now, they aren't needed
      imports = readImports(f)
    elseif id == 3 # Functions
      f_types = readFuncTypes(f)
    elseif id == 5 # Memory
      memory = readMemory(f)
      @show memory
    elseif id == 7 # Exports
      exports = readExports(f)
    elseif id == 6
      globals = readGlobals(f)
    elseif id == 8
      start_index = readLeb128(f, UInt32) + 1
    elseif id == 10 # Bodies, do this later when names are sorted
      bodies_f = position(f)
      skip(f, payload_len)
    elseif id == 11 # Data segments
      data = readData(f)
    else
      error("Unknown Section: $id")
    end
  end
  names = getNames(names, [(:func, length(f_types) + length(imports)), (:memory, length(memory))])
  # show(names[:func][1])

  bodies_f != -1 || error("No code in file")
  bodies = bodiesToCode(seek(f, bodies_f), names[:func])

  length(bodies) == length(f_types) || error("Number of function types and function bodies does not match.")
  l = length(imports)+1
  # @show l
  # @show length(f_types)
  funcs   = [Func(n, types[t+1]..., b...) for (n, t, b) in zip(names[:func][l:end], f_types, bodies)]
  # @show names[:memory]
  mems    = [Mem(n, m...) for (n, m) in zip(names[:memory], memory)]
  # @show mems
  exports = [Export(n, names[is][i+1], is) for (n, is, i) in exports]
  func_imports = [Import(m, fs, n, k, FuncType(types[t+1]...)) for (n, (m,fs,k,t)) in zip(names[:func][1:length(imports)],imports)]

  start = start_index == -1 ? nothing : (start_index > length(func_imports) ? funcs[start_index - length(func_imports)] : func_imports[start_index]).name
  # @show start
  # @show data
  return Module([], funcs, [], mems, globals, [], data, start, func_imports, exports)
end

function getNames(ns, sl)
  Dict(s => [get(ns[s], i, Symbol(s,"_$i")) for i in 0:l-1] for (s, l) in sl)
end

getRegisters(f) = [types_r[b] for b in readBytes(f)]

getNumArray(f, typ, vs=Vector{typ}()) = readArray(()->readLeb128(f, typ), f, vs)

function readLeb128(f, typ=Int32)
  bs = read(f, 1)
  while (bs[end] & 0x80 != 0)
    push!(bs, read1(f))
  end
  return fromLeb128(bs, typ)
end

readBytes(f) = read(f, readLeb128(f, UInt32))
readutf8(f) = f |> readBytes |> String
readsymbol(f) = f |> readBytes |> Symbol

function readBody(f, fns, no_result=false)
  is = Vector{Instruction}()
  result = no_result ? Void() : types_r[read1(f)]
  b = read1(f)
  while (b != opcodes[:end]) && (b != opcodes[:else])
    op = readOp(opcodes_r[b], f, fns) :: Instruction
    push!(is, op)
    b = read1(f)
  end
  return is, result, b == opcodes[:else] ? readBody(f, fns, true)[1] : []
end

function readOp(block :: DataType, f, fns)
  is, r, f_is = readBody(f, fns)
  return block == If ? If(is, f_is, r) : block(is, r)
end

readOp(x::WebAssembly.Instruction, f, _) = x

function readOp(x :: Tuple{DataType, Any}, f, fns)
  x[1] == Const && return Const(readLeb128(f, jltype(x[2][1])))
  arg = readLeb128(f, UInt32)
  x[1] == Call && return Call(fns[arg + 1])
  if x[1] == MemoryOp
    offset = readLeb128(f, UInt32)
    alignment = UInt(2)^arg # Space in arg for more flags, currently all 0.
    return MemoryOp(x[2]...,offset,alignment)
  end
  return x[1](x[2]...,arg)
end

# Given a function for reading an item, read an array of those items.
# Assumes length of the array followed by the array itself is stored.
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

###############################################################################
################                 Lookup Tables                 ################
###############################################################################

const types =
  Dict(
    i32      => 0x7f,
    i64      => 0x7e,
    f32      => 0x7d,
    f64      => 0x7c,
    :anyfunc => 0x70,
    :func    => 0x60,
    Void()   => 0x40
  )

const types_r = map(reverse, types)

const opcodes =
  Dict(

    Return()      => 0x0f,
    Select()      => 0x1b,
    Unreachable() => 0x00,
    Nop()         => 0x01,

    (Local, ()) => 0x20,

    (SetLocal, (true,))  => 0x22,
    (SetLocal, (false,)) => 0x21,

    (SetGlobal, ())   => 0x24,
    (GetGlobal, ())   => 0x23,

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
    Convert(f64, i64, :reinterpret)  =>	0xbf,

    (MemoryOp, (i32, :load))     =>	0x28,
    (MemoryOp, (i64, :load))     =>	0x29,
    (MemoryOp, (f32, :load))     =>	0x2a,
    (MemoryOp, (f64, :load))     =>	0x2b,
    (MemoryOp, (i32, :load, Int8))  =>	0x2c,
    (MemoryOp, (i32, :load, UInt8))  =>	0x2d,
    (MemoryOp, (i32, :load, Int16)) =>	0x2e,
    (MemoryOp, (i32, :load, UInt16)) =>	0x2f,
    (MemoryOp, (i64, :load, Int8))  =>	0x30,
    (MemoryOp, (i64, :load, UInt8))  =>	0x31,
    (MemoryOp, (i64, :load, Int16)) =>	0x32,
    (MemoryOp, (i64, :load, UInt16)) =>	0x33,
    (MemoryOp, (i64, :load, Int32)) =>	0x34,
    (MemoryOp, (i64, :load, UInt32)) =>	0x35,
    (MemoryOp, (i32, :store))    =>	0x36,
    (MemoryOp, (i64, :store))    =>	0x37,
    (MemoryOp, (f32, :store))    =>	0x38,
    (MemoryOp, (f64, :store))    =>	0x39,
    (MemoryOp, (i32, :store, Int8))   =>	0x3a,
    (MemoryOp, (i32, :store, Int16))  =>	0x3b,
    (MemoryOp, (i64, :store, Int8))   =>	0x3c,
    (MemoryOp, (i64, :store, Int16))  =>	0x3d,
    (MemoryOp, (i64, :store, Int32))  =>	0x3e,

    (MemoryUtility, (:current_memory,)) =>	0x3f,
    (MemoryUtility, (:grow_memory,))    =>	0x40

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
