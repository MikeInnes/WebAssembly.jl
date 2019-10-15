module Wasmer

using Libdl

function __init__()
  @eval const wasmer = dlopen(joinpath(@__DIR__, "..", "deps", "wasmer"))
end

@enum ExportKind begin
  Function
  Global
  Memory
  Table
end

@enum Result begin
  Ok = 1
  Error = 2
end

@enum ValueTag begin
  I32
  I64
  F32
  F64
end

struct ByteArray
  bytes::Ptr{UInt8}
  len::UInt32
end

end
