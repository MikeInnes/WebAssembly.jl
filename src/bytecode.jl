# Converts a module to its bytecode representation.

# The bytecode layout is somewhat different to the structure of the wasm IR. It
# therefore makes sense to restructure the IR into an intermediary format and
# then make a short hop to bytecode. The length of each section needs to be
# saved alongside it which would require a second pass anyway.

const magic   = UInt32(0x6d736100) # \0asm
const version = UInt32(0x1)        # Targeting version 1

# Types
# TODO: Add support for FuncType

# Converts any Integer type to Leb128 format as an array of bytes.
function toLeb128(x :: Integer)
  len = sizeof(x) * 8
  bytes = []

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

# Take a module and return a dictionary FuncType ->
# function getTypes()
