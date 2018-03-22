# using Base.Test
# using WebAssembly
# using WebAssembly.Instructions

@testset "Interpreter" begin
using Charlotte

function rand_test_wasm(f, wasm_f, n_tests = 50)
  for i in 1:n_tests
    args = [rand(WebAssembly.jltype(typ)) for typ in wasm_f.params]
    if WebAssembly.interpretwasm(wasm_f, args)[1] != f(args...)
      return false
    end
  end
  return true
end

relu(x) = ifelse(x > 0, x, 0)
wasm_relu = @code_wasm relu(1)
@test rand_test_wasm(relu, wasm_relu)

orelu(x) = ifelse(x < 0, x, 0)
wasm_orelu = @code_wasm orelu(1)
@ test rand_test_wasm(orelu, wasm_orelu)

function loop_relu(x)
  while x < 0 && x > -1000
    x = x + 1
  end
  return x
end

wasm_loop = @code_wasm loop_relu(1)
@test rand_test_wasm(loop_relu, wasm_loop)

function relu_if(x)
  if x < 0
    x = 0
  end
  return x
end

wasm_loop = @code_wasm relu_if(1)
@test rand_test_wasm(relu_if, wasm_loop)

function loop_relu_early(x)
  while x < 0 && x > -1000
    x = x + 1
    if x == -10
      return 10
    end
  end
  return x
end

wasm_loop = @code_wasm loop_relu_early(1)
@test rand_test_wasm(loop_relu_early, wasm_loop)

function divide(x, y, z)
  return float(x * y) / float(y * z)
end

wasm_divide = @code_wasm divide(1, 2, 3)
@test rand_test_wasm(divide, wasm_divide)

function sumn(n)
  x = 0
  if n > 100 
    n = 100
  end
  for i = 1:n
    x += i
  end
  return x
end

wasm_sumn = @code_wasm sumn(1)
@test rand_test_wasm(sumn, wasm_sumn)

wasm_gcd = @code_wasm gcd(5, 10)
# @test rand_test_wasm(gcd, wasm_gcd)

end