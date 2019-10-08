export write_wasm, write_wast, wast2wasm, wasm2wast

"""
    write_wast(filename, m::Module)

Write the WebAssembly module `m` to WebAssembly text format in `filename`.
"""
function write_wast(filename, m::Module)
  open(filename, "w") do f
    show(f, m)
  end
end

"""
    write_wast(filename, m::Module)

Write the WebAssembly module `m` to WebAssembly binary format in `filename`.
"""
function binary(m::Module, file)
  wast = tempname() * ".wast"
  write_wast(wast, m)
  run(`$(WABT.wast2wasm) $wast -o $file`)
  run(`$(Binaryen.wasm_opt) $file -O4 -o $file.opt`)
  rm(wast)
  return
end
