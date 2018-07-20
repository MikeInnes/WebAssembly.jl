# Helper library to produce a js module that reads in wasm library.

const wtype = """
function wtype(i) {
  var t = typeof(i)
  if (t == "number") {
    return Number.isInteger(i) ? 'i32' : 'f64'
  } else if (t == "bigint") {
    return 'i64';
  }
  throw "Unsupported Type: " + i + " :: " + t;
}
"""

ffiType(t::Type{Int64}) = "'i64'"
ffiType(t::Type{Int32}) = "'i32'"
ffiType(t::Type{Float32}) = "'f32'"
ffiType(t::Type{Float64}) = "'f64'"
ffiType(t) = string("[", ffiType(t[2][1]), ", [", join(map(ffiType, t[1]), ", "), "]]")

ffiType(t::Int) = t

function wrapper(efs)
  wrapper = ["""
  const library = new ffi.Wrapper({
  ""","""\n
  }, {
    dialect: 'assemblyscript',
  });
  """]
  fs = [string("  ", n, ": ", ffiType(f.typ)) for (n, f) in efs]
  return string(wrapper[1], join(fs, ", \n"), wrapper[2])

end

function exportfs(m)
  fs = Dict(f.name => f for f in m.funcs)
  es = filter(e -> e.typ == :func, m.exports)
  return map(e -> e.name,es), Dict(e.name => fs[e.internalname] for e in m.exports)
end

function fetch_(names)
  fetch = ["""
  library.fetch_ = library.fetch;

  library.fetch = (f => library.fetch_(f).then(() => {
  ""","""\n
  }));
  """]
  defs = [string("  ", n, " = library.", n, ".bind(library);") for n in names]
  # fetch = ["library.fetch('", "').then(() => {\n}).catch(console.error);"]
  return join(fetch, join(defs, "\n"))
end

function getbasename(n, f)
  snd = join(WType.(f.typ[1]), "_")
  length(n) < length(snd) && return n
  if n[end-length(snd)+1:end] == snd
    return n[1:end-length(snd)-1]
  end
  return n
end

function jsmodule(m)
  names, efs = exportfs(m)
  defs = wrapper(efs)
  ns = join(names, ", ")
  fetch = fetch_(names)

  @show getbasename("addTwo_i32_i32", efs[:addTwo])

  vars = string("var ", ns, ";\n");
  exports = string("export { ", ns, " };\nexport default library;\n")

  # wrapper(efs)
  join([defs, vars, fetch, exports], "\n")
end
