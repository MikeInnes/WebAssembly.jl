
function write_wast(filename, m::Module)
  open(filename, "w") do f
    show(io, m)       
  end
end

function write_wasm(filename, m::Module)
  wastname = tempname() * ".wast"
  write_wast(wastname, m)
  run(`$wast2wasm $wastname -o $filename`)
end
