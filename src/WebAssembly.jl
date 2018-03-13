module WebAssembly

include("wasm.jl")
include("passes.jl")
include("looper.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Return, Trap, Label, Goto, nop, trap

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Return, Trap, Label, Goto, nop, trap

end

end # module
