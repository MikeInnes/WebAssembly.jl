using Base.Meta

function code_wasm(ex, A)
  cinfo, R = code_typed(ex, A)[1]
  body = Compile.towasm_(Compile.lower(cinfo)) |> restructure
  Func([WType(T) for T in A.parameters],
       [WType(R)],
       [WType(P) for P in cinfo.slottypes[length(A.parameters)+2:end]],
       body)
end

macro code_wasm(ex)
  isexpr(ex, :call) || error("@code_wasm f(xs...)")
  :(code_wasm($(esc(ex.args[1])), Base.typesof($(esc.(ex.args[2:end])...))))
end
