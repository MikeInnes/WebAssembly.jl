module WebAssembly

# All of the WABt utilities are accessed through the following module:
module WABT
include("../deps/wabt_deps.jl")
end

module Binaryen
include("../deps/binaryen_deps.jl")
end

function __init__()
    WABT.check_deps()
end

include("wasm.jl")
include("ir.jl")
include("passes.jl")
include("looper.jl")
include("io.jl")
include("parser.jl")
include("interpret.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, Label, Goto, nop, unreachable

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, Label, Goto, nop, unreachable

end

end # module
