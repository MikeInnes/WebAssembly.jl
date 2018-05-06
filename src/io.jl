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
function write_wasm(filename, m::Module)
  wastname = tempname() * ".wast"
  write_wast(wastname, m)
  wast2wasm(wastname, filename)
end

"""
    wast2wasm(input, output)

Convert the text WebAssembly file named `input` to the binary WebAssembly file `output`.
"""
wast2wasm(input, output) = run(`$(WABT.wast2wasm) $input -o $output`)

"""
    wasm2wast(input, output)

Convert the binary WebAssembly file named `input` to the text WebAssembly file `output`.
"""
wasm2wast(input, output) = run(`$(WABT.wasm2wast) $input -o $output`)