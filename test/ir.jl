using WebAssembly, IRTools.All, Test
using WebAssembly: irfunc, f64, i64, i32

add = let
  ir = IR()
  a = argument!(ir, f64)
  b = argument!(ir, f64)
  c = push!(ir, stmt(xcall(f64.add, a, b), type = f64))
  return!(ir, c)
end

func = irfunc(:foo, copy(add))

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

pow = let
  ir = IR()
  x = argument!(ir, f64)
  n = argument!(ir, i64)
  cond = block!(ir)
  body = block!(ir)
  ret  = block!(ir)
  n = argument!(cond, n)
  r = argument!(cond, 0.0)
  branch!(cond, ret, unless = push!(cond, stmt(xcall(i64.gt, n, 0), type = i32)))
  n = push!(body, stmt(xcall(i64.sub, n, 1), type = i64))
  r′ = push!(body, stmt(xcall(f64.mul, r, x), type = f64))
  branch!(body, cond, n, r′)
  return!(ret, r)
  ir
end
