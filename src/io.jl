export write_wasm, write_wast

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
  run(`$wast2wasm $wastname -o $filename`)
end
