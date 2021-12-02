module WebAssembly

include("wasm.jl")
include("ir.jl")
include("passes.jl")
include("io.jl")
include("parser.jl")
include("interpret.jl")

include("passes/multi.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, nop, unreachable

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, Label, Goto, nop, unreachable

end

end # module
