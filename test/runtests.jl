using WebAssembly
using WebAssembly.Instructions
using Base.Test

@testset "WebAssembly" begin

b = Block([Nop(), Nop()]) |> WebAssembly.nops
@test isempty(b.body)

end
