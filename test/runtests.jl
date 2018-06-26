using WebAssembly
using WebAssembly.Instructions
using WebAssembly: parsewast, interpretwasm, interpret_module, WType, Func, Module, FuncType, Func, Table, Mem, Global, Elem, Data, Import, Export, i32, i64, f32, f64
using Base.Test


@testset "WebAssembly" begin

b = Block([Nop(), Nop()]) |> WebAssembly.nops
@test isempty(b.body)

include("interpret.jl")

end

@testset "Bytecode" begin

using WebAssembly: fromLeb128, toLeb128

@test fromLeb128(toLeb128(-1), Int8) == -1
@test fromLeb128(toLeb128( 0), Int8) == 0
@test fromLeb128(toLeb128(UInt8(0)), UInt8) == 0

for t ∈ [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
  pass = true
  for i in 1:100
    x = rand(t)
    if !(fromLeb128((toLeb128(x)), t) == x) || !(fromLeb128((toLeb128(x))) == x)
      pass = false
      break
    end
  end
  @test pass
end

end

@testset "Import-export" begin

m = WebAssembly.Module(FuncType[], Func[], Table[], Mem[], Global[], Elem[], Data[], Ref(0),
           [Import(:env, :mathfun, :func, [i32, f64], f64)],
           [Export(:fun, :fun, :func)])

end
