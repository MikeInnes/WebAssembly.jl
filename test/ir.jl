using WebAssembly, IRTools.All, Test
using WebAssembly: irfunc, f64, i64, i32

add = let
  ir = IR()
  a = argument!(ir, f64)
  b = argument!(ir, f64)
  c = push!(ir, stmt(xcall(f64.add, a, b), type = f64))
  return!(ir, c)
end

func = irfunc(:add, copy(add))

@test WebAssembly.interpretwasm(func, Dict(), [2, 3]) == [5.0]

relu = let
  ir = IR()
  cond = block(ir, 1)
  els = block!(ir)
  x = argument!(ir, f64)
  ltz = push!(cond, stmt(xcall(f64.gt, x, 0.0), type = i32))
  branch!(cond, els, unless = ltz)
  return!(cond, x)
  return!(els, 0.0)
  ir
end

func = irfunc(:relu, copy(relu))

@test WebAssembly.interpretwasm(func, Dict(), [5]) == [5]
@test WebAssembly.interpretwasm(func, Dict(), [-5]) == [0]

pow = let
  ir = IR()
  x = argument!(ir, f64)
  n = argument!(ir, i32)
  cond = block!(ir)
  body = block!(ir)
  ret  = block!(ir)
  n = argument!(cond, n)
  r = argument!(cond, 1.0)
  branch!(cond, ret, unless = push!(cond, stmt(xcall(i32.gt_s, n, Int32(0)), type = i32)))
  n = push!(body, stmt(xcall(i32.sub, n, Int32(1)), type = i32))
  r′ = push!(body, stmt(xcall(f64.mul, r, x), type = f64))
  branch!(body, cond, n, r′)
  return!(ret, r)
  ir
end

func = irfunc(:pow, copy(pow))
@test WebAssembly.interpretwasm(func, Dict(), [2, 3]) == [8]
