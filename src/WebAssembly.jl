module WebAssembly

using Base.Meta

export @code_wasm

include("wasm/wasm.jl")
include("wasm/looper.jl")

include("julia/compile.jl")
include("julia/interface.jl")

end # module
