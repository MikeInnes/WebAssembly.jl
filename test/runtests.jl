using WebAssembly
using WebAssembly.Instructions
using WebAssembly: WType, Func, Module, FuncType, Func, Table, Mem, Global, Elem, Data, Import, Export, i32, f64
using Base.Test

@testset "WebAssembly" begin

b = Block([Nop(), Nop()]) |> WebAssembly.nops
@test isempty(b.body)

end

@testset "Import-export" begin

m = Module(FuncType[], Func[], Table[], Mem[], Global[], Elem[], Data[], Ref(0),
           [Import(:env, :mathfun, :func, [i32, f64], f64)],
           [Export(:fun, :fun, :func)])

end
