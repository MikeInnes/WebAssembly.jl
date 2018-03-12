module WebAssembly

include("wasm.jl")
include("passes.jl")
include("looper.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select,
  Block, If, Loop, Branch, Return, Label, Goto

export Instruction, Const, Nop, Local, SetLocal, Op, Select,
  Block, If, Loop, Branch, Return, Label, Goto

end

end # module
