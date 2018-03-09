macro code_wasm(ex)
  isexpr(ex, :call) || error("@code_wasm f(xs...)")
  :(code_wasm($(esc(ex.args[1])), Base.typesof($(esc.(ex.args[2:end])...))))
end
