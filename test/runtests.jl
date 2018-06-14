using WebAssembly
using WebAssembly.Instructions
using WebAssembly: parsewast, interpretwasm, interpret_module, WType, Func, Module, FuncType, Func, Table, Mem, Global, Elem, Data, Import, Export, i32, i64, f32, f64
using Base.Test


@testset "WebAssembly" begin

b = Block([Nop(), Nop()]) |> WebAssembly.nops
@test isempty(b.body)

include("interpret.jl")

end

@testset "Import-export" begin

m = WebAssembly.Module(FuncType[], Func[], Table[], Mem[], Global[], Elem[], Data[], Ref(0),
           [Import(:env, :mathfun, :func, [i32, f64], f64)],
           [Export(:fun, :fun, :func)])

end
