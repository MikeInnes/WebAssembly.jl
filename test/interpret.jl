# using Base.Test
# using WebAssembly
# using WebAssembly.Instructions

using MacroTools: prewalk, postwalk

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

tests = @pair_with_names [ relu_ifelse
                         , relu_ternary
                         , relu_if_then_else
                         , pow
                         ]

@testset "Interpreter" begin

function rand_test_wasm(f, wasm_f, n_tests = 50, max = 100)
  for i in 1:n_tests
    args = [rand(WebAssembly.jltype(typ)) % max for typ in wasm_f.params]
    WebAssembly.interpretwasm(wasm_f, args)[1] != f(args...) && return false
  end
  return true
end

relu_wasm = parsewast("test/wast/functions/relu_ifelse.wast")

# This block isn't especially necessary
relu_wasm_expected = Func(Symbol("#relu_Int64"), [i64], [i64], [], Block([Const(0), Local(0), Local(0), Const(0), Op(i64, :lt_s), Select(), Return()]))
@test relu_wasm.body.body == relu_wasm_expected.body.body
@test relu_wasm.params == relu_wasm_expected.params
@test relu_wasm.returns == relu_wasm_expected.returns

# This test is the one that matters
@test rand_test_wasm(relu_ifelse, relu_wasm)

# Bulk form for functions

root = "test/wast/functions/"

for test in tests
  test_wasm = parsewast(root * test[2] * ".wast")
  @test rand_test_wasm(test[1], test_wasm)
end



# function loop_relu(x)
#   while x < 0 && x > -1000
#     x = x + 1
#   end
#   return x
# end

# wasm_loop = @code_wasm loop_relu(1)
# @test rand_test_wasm(loop_relu, wasm_loop)

# wasm_loop = @code_wasm relu_if(1)
# @test rand_test_wasm(relu_if, wasm_loop)

# function loop_relu_early(x)
#   while x < 0 && x > -1000
#     x = x + 1
#     if x == -10
#       return 10
#     end
#   end
#   return x
# end

# wasm_loop = @code_wasm loop_relu_early(1)
# @test rand_test_wasm(loop_relu_early, wasm_loop)

# function divide(x, y, z)
#   return float(x * y) / float(y * z)
# end

# wasm_divide = @code_wasm divide(1, 2, 3)
# @test rand_test_wasm(divide, wasm_divide)

# function sumn(n)
#   x = 0
#   if n > 100
#     n = 100
#   end
#   for i = 1:n
#     x += i
#   end
#   return x
# end

# wasm_sumn = @code_wasm sumn(1)
# @test rand_test_wasm(sumn, wasm_sumn)

# wasm_gcd = @code_wasm gcd(5, 10)
# # @test rand_test_wasm(gcd, wasm_gcd)

end
