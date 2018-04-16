module WebAssembly

include("../deps/deps.jl")

function __init__()
    check_deps()
end

include("wasm.jl")
include("passes.jl")
include("looper.jl")
include("io.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, Label, Goto, nop, unreachable

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, Label, Goto, nop, unreachable

end

end # module
