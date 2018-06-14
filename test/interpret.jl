using MacroTools: postwalk

macro pair_with_names(xs)
  postwalk(xs) do x
    !(x isa Expr) ? :($x, $(string(x))) : x
  end
end

relu_ifelse(x) = ifelse(x > 0, x, 0)
relu_ternary(x) = x > 0 ? x : 0
relu_if_then_else = relu_ifelse
function pow(x, n)
  r = 1
  while n > 0
    r *= x
    n -= 1
  end
  return r
end
addTwo(x, y) = x + y

tests = @pair_with_names [ relu_ifelse
                         , relu_ternary
                         , relu_if_then_else
                         , pow
                         , addTwo
                         ]

function rand_test_wasm(f, wasm_f, n_tests = 50, max = 100)
 for i in 1:n_tests
   args = [rand(WebAssembly.jltype(typ)) % max for typ in wasm_f.params]
   WebAssembly.interpretwasm(wasm_f, args)[1] != f(args...) && return false
 end
 return true
end

@testset "Parse-Interpret" begin

relu_wasm = "test/wast/functions/relu_ifelse.wast" |> WebAssembly.getFileParseBrackets |> WebAssembly.func

relu_wasm_expected = Func(Symbol("#relu_Int64"), [i64], [i64], [], Block([Const(0), Local(0), Local(0), Const(0), Op(i64, :lt_s), Select(), Return()]))
@test relu_wasm.body.body == relu_wasm_expected.body.body
@test relu_wasm.params == relu_wasm_expected.params
@test relu_wasm.returns == relu_wasm_expected.returns
@test relu_wasm.name == relu_wasm_expected.name

@test rand_test_wasm(relu_ifelse, relu_wasm)

# Bulk form for functions
root = "test/wast/functions/"

for test in tests
  test_wasm = root * test[2] * ".wast" |> WebAssembly.getFileParseBrackets |> WebAssembly.func
  @test rand_test_wasm(test[1], test_wasm)
end

# Sort of test module parsing
m = parsewast("test/wast/modules/addTwo.wast")
@test m.exports == [Export(:addTwo, :addTwo, :func)]
expected_func = Func(Symbol("addTwo"), [i32, i32], [i32], [], Block([Local(0), Local(1), Op(i32, :add)]))
@test m.funcs[1].body.body == expected_func.body.body
@test m.funcs[1].params == expected_func.params
@test m.funcs[1].returns == expected_func.returns
@test m.funcs[1].name == expected_func.name

end
