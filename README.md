# WebAssembly

[![Build Status](https://travis-ci.org/MikeInnes/WebAssembly.jl.svg?branch=master)](https://travis-ci.org/MikeInnes/WebAssembly.jl)

Tools for working with the [WebAssembly](http://webassembly.org/) format in Julia. In particular, this package exposes the IR as simple Julia data structures, allowing parsers and code generators to convert to/from the IR, as well as allowing optimisation passes directly on the IR in the vein of [binaryen](https://github.com/WebAssembly/binaryen).

Build IR for a `x^n` function:

```julia
using WebAssembly: i32, f64, irfunc
using IRTools.All

pow = let
  ir = IR()
  x = argument!(ir, f64)
  n = argument!(ir, i32)
  cond = block!(ir)
  body = block!(ir)
  ret  = block!(ir)
  n = argument!(cond, n, i32)
  r = argument!(cond, 1.0, i64)
  branch!(cond, ret, unless = push!(cond, stmt(xcall(i32.gt_s, n, Int32(0)), type = i32)))
  n = push!(body, stmt(xcall(i32.sub, n, Int32(1)), type = i32))
  r′ = push!(body, stmt(xcall(f64.mul, r, x), type = f64))
  branch!(body, cond, n, r′)
  return!(ret, r)
  ir
end
```

```julia
julia> pow
1: (%1 :: f64, %2 :: i32)
  br 2 (%2, 1.0)
2: (%3 :: i32, %4 :: f64)
  %5 = (i32.gt_s)(%3, 0) :: i32
  br 4 unless %5
3:
  %6 = (i32.sub)(%3, 1) :: i32
  %7 = (f64.mul)(%4, %1) :: f64
  br 2 (%6, %7)
4:
  return %4
```

Construct a wasm function and module:

```julia
func = irfunc(:pow, pow)

mod = WebAssembly.Module(funcs=[func],
   exports=[WebAssembly.Export(:pow, :pow, :func)])

WebAssembly.binary(mod, "test.wasm")
```

Disassemble the result with binaryen:

```wasm
shell> wasm-dis test.wasm
(module
 (type $0 (func (param f64 i32) (result f64)))
 (export "pow" (func $0))
 (func $0 (; 0 ;) (type $0) (param $0 f64) (param $1 i32) (result f64)
  (local $2 f64)
  (local.set $2 (f64.const 1))
  (loop $label$1
   (if
    (i32.eqz (i32.le_s (local.get $1) (i32.const 0)))
    (block
     (local.set $1
      (i32.sub (local.get $1) (i32.const 1)))
     (local.set $2
      (f64.mul (local.get $2) (local.get $0)))
     (br $label$1))))
  (local.get $2)))
```

(If wasm-dis is not on your path you can use

```julia
julia> run(`$(WebAssembly.Binaryen.wasm_dis) test.wasm`);
```

to get syntax-highlighted WASM in a terminal).
