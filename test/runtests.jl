using WebAssembly
using WebAssembly.Instructions
using WebAssembly: WType, Func, Module, Import, Export, i32, f64
using Base.Test

@testset "WebAssembly" begin

b = Block([Nop(), Nop()]) |> WebAssembly.nops
@test isempty(b.body)

end

@testset "Import-export" begin

m = Module([Import(:env, :mathfun, :func, [i32, f64])],
           [Export(:fun, :func),
            Export(:fun2, :func)],
           Func[])


end
