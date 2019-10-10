"""
    write_wat(filename, m::Module)

Write the WebAssembly module `m` to WebAssembly text format in `filename`.
"""
function write_wat(filename, m::Module)
  open(filename, "w") do f
    show(f, m)
  end
end

"""
    write_wat(filename, m::Module)

Write the WebAssembly module `m` to WebAssembly binary format in `filename`.
"""
function binary(m::Module, file)
  wat = tempname() * ".wat"
  write_wat(wat, m)
  run(`$(WABT.wat2wasm) $wat -o $file`)
  run(`$(Binaryen.wasm_opt) $file -O4 -o $file`)
  rm(wat)
  return
end
