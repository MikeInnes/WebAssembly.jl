base = "https://github.com/wasmerio/wasmer/releases/download/0.8.0/libwasmer_runtime_c_api"

ext =
  Sys.isapple() ? "dylib" :
  Sys.iswindows() ? "dll" :
  "so"

download("$base.$ext", joinpath(@__DIR__, "wasmer.$ext"))
