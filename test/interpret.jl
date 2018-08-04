function pair_with_wasm(xs)
  [(eval(x), eval(Symbol(x, :_wasm))) for x in xs]
end

macro wast_str(wast)
  return wast |> WebAssembly.parsebrackets |> WebAssembly.parse
end

relu_ifelse(x) = ifelse(x > 0, x, 0)
relu_ifelse_wasm = wast"""
(func $#relu_Int64  (param i64) (result i64)
  (i64.const 0)
  (get_local 0)
  (get_local 0)
  (i64.const 0)
  (i64.lt_s)
  (select)
  (return))
"""

relu_ternary(x) = x > 0 ? x : 0
relu_ternary_wasm = wast"""
(func $#relu_ternary_Int64  (param i64) (result i64)
  (i64.const 0)
  (get_local 0)
  (i64.lt_s)
  (if
    (then
      (get_local 0)
      (return)))
  (i64.const 0)
  (return))
"""

relu_if_then_else = relu_ifelse
relu_if_then_else_wasm = wast"""
(func $#relu_if_then_else_Int64  (param i64) (result i64)
  (i64.const 0)
  (get_local 0)
  (i64.lt_s)
  (if
    (then
      (get_local 0))
    (else
      (i64.const 0)))
  (return))
"""

function pow(x, n)
  r = 1
  while n > 0
    r *= x
    n -= 1
  end
  return r
end
pow_wasm = wast"""
(func $#pow_Int64_Int64  (param i64) (param i64) (result i64)
  (local i64) (local i64)
  (get_local 1)
  (set_local 3)
  (i64.const 1)
  (set_local 2)
  (block
    (loop
      (i64.const 0)
      (get_local 3)
      (i64.lt_s)
      (i32.eqz)
      (br_if 1)
      (get_local 2)
      (get_local 0)
      (i64.mul)
      (set_local 2)
      (get_local 3)
      (i64.const 1)
      (i64.sub)
      (set_local 3)
      (br 0)))
  (get_local 2)
  (return))
"""

addTwo(x, y) = x + y
addTwo_wasm = wast"""
(func $addTwo (param i32) (param i32) (result i32)
  (get_local 0)
  (get_local 1)
  (i32.add))
"""

tests = pair_with_wasm([ :relu_ifelse
                       , :relu_ternary
                       , :relu_if_then_else
                       , :pow
                       , :addTwo
                       ])

fib(x) = x <= 1 ? 1 : fib(x - 1) + fib(x - 2)
this(x) = pow(x + 1, x - 1)

function rand_test_wasm(f, wasm_f, n_tests = 50, max = 100)
  for i in 1:n_tests
    args = [rand(WebAssembly.jltype(typ)) % max for typ in wasm_f.params]
    WebAssembly.interpretwasm(wasm_f, Dict(), args)[1] != f(args...) && return false
  end
  return true
end

function rand_test_module(fs, m, n_tests = 50, max = 10)
  wasm_fs = interpret_module(m)
  @show length(wasm_fs)
  @show length(fs)
  for i in eachindex(fs)
    for j in 1:n_tests
      args = [rand(WebAssembly.jltype(typ)) % WebAssembly.jltype(typ)(max) for typ in m.funcs[i].params]
      wasm_fs[i](args...)[1] != fs[i](args...) && return false
    end
  end
  return true
end

m = wast"""
(module
  (func $addTwo (param i32) (param i32) (result i32)
    (get_local 0)
    (get_local 1)
    (i32.add))
  (export "addTwo" (func $addTwo)))
"""
m2 = wast"""
(module
  (export "this" (func $#this_Int64))
  (export "pow" (func $#pow_Int64_Int64))
  (export "fib" (func $#fib_Int64))
  (memory 1)
  (func $#fib_Int64 (param i64) (result i64)
    (get_local 0)
    (i64.const 1)
    (i64.le_s)
    (if
      (then
        (i64.const 1)
        (return)))
    (get_local 0)
    (i64.const 1)
    (i64.sub)
    (call $#fib_Int64)
    (get_local 0)
    (i64.const 2)
    (i64.sub)
    (call $#fib_Int64)
    (i64.add)
    (return))
  (func $#this_Int64 (param i64) (result i64)
    (get_local 0)
    (i64.const 1)
    (i64.add)
    (get_local 0)
    (i64.const 1)
    (i64.sub)
    (call $#pow_Int64_Int64)
    (return))
  (func $#pow_Int64_Int64 (param i64) (param i64) (result i64)
  (local i64) (local i64)
    (get_local 1)
    (set_local 3)
    (i64.const 1)
    (set_local 2)
    (block
      (loop
        (i64.const 0)
        (get_local 3)
        (i64.lt_s)
        (i32.eqz)
        (br_if 1)
        (get_local 2)
        (get_local 0)
        (i64.mul)
        (set_local 2)
        (get_local 3)
        (i64.const 1)
        (i64.sub)
        (set_local 3)
        (br 0)))
    (get_local 2)
    (return)))
"""

@testset "Parse-Interpret" begin

relu_wasm = relu_ifelse_wasm

relu_wasm_expected = Func(Symbol("#relu_Int64"), [i64], [i64], [], Block([Const(0), Local(0), Local(0), Const(0), Op(i64, :lt_s), Select(), Return()]))
@test relu_wasm.body.body == relu_wasm_expected.body.body
@test relu_wasm.params == relu_wasm_expected.params
@test relu_wasm.returns == relu_wasm_expected.returns
@test relu_wasm.name == relu_wasm_expected.name

@test rand_test_wasm(relu_ifelse, relu_wasm)

# Bulk form for functions
root = "test/wast/functions/"

for test in tests
  @test rand_test_wasm(test[1], test[2])
end

# Sort of test module parsing
@test m.exports == [Export(:addTwo, :addTwo, :func)]
expected_func = Func(Symbol("addTwo"), [i32, i32], [i32], [], Block([Local(0), Local(1), Op(i32, :add)]))
@test m.funcs[1].body.body == expected_func.body.body
@test m.funcs[1].params == expected_func.params
@test m.funcs[1].returns == expected_func.returns
@test m.funcs[1].name == expected_func.name

@test m2.exports == [Export(:this, Symbol("#this_Int64"), :func), Export(:pow, Symbol("#pow_Int64_Int64"), :func), Export(:fib, Symbol("#fib_Int64"), :func)]

# map!(WebAssembly.optimise, m2.funcs)
@test rand_test_module([fib, this, pow], m2)

end


@testset "Bytecode" begin
  using WebAssembly: getModule, readModule, State
  io = IOBuffer()
  write(io, getModule(m2))
  @show typeof(io)
  m2_ = io |> seekstart |> readModule
  @test m2_.exports == m2.exports
  @test m2_.mems == m2.mems
  @test rand_test_module([fib, this, pow], m2)

  @testset "Interpret" begin
    begin
      # Initialisation test
      using WebAssembly: getModule, readModule
      io = IOBuffer()
      m = readModule(joinpath(dirname(@__FILE__), "base.wasm"))
      s = State(m)
      @test length(s.fs) == length(m.funcs)
      @test length(s.mem[1]) == m.mems[1].min * 65536

      efs = filter(e->e.typ==:func, m.exports)
      defs = Dict(e.name => (xs...) -> s.fs[e.internalname][2](xs...)[1] for e in efs)
      p = defs[:allocate](Int32(100))
      g = defs[:allocate](Int32(100))
      @test defs[:arraylen_i32](p) == 0
      @test defs[:arrayset_i32](p, Int32(3), Int32(0)) == p
      @test defs[:arrayref_i32](p, zero(Int32)) == 3
      @test defs[:arraylen_i32](p) == 1
      @test defs[:arrayset_i32](p,Int32(3),Int32(394)) == p
      @test defs[:arrayref_i32](p,Int32(394)) == 3
      @test defs[:arraylen_i32](p) == 395
    end
    ####






  end

end

  # write(io, getModule())
