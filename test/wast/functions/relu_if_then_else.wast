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
