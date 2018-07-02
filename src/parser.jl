parsewast(filename) = filename |> getFileParseBrackets |> module_

getFileParseBrackets(filename) = filename |> getFileString |> parsebrackets

function getFileString(filename)
  f = open(filename)
  s = readstring(f)
  close(f)
  return s
end

parsebrackets(s) = pb(s, 1)[1] |> deNest

function pb(s, i)
  j = i
  result = []
  while j < length(s)
    if s[j] == '('
      (b, i_) = pb(s, j + 1)
      j > i && push!(result, s[i:j-1])
      push!(result, b)
      j = i_
      i = i_ + 1
    elseif s[j] == ')'
      return (push!(result, s[i:j-1]), j)
    end
    j = j + 1
  end
  j > i && push!(result, s[i:j - 1])
  return (result, j)
end

function parse(wast)
  p = wast[1] isa String ? wast[1] : wast[1][1]
  p == "func"   && return func(wast)
  p == "module" && return module_(wast)
end


rmLayer(xs::Array) = length(xs) == 1 ? xs[1] : xs

# Split strings, remove white space and excess nestings of arrays.
# [[["func $this", ["get_local 1"]]]] -> [["func", "$this"], ["get_local", "1"]]
function deNest(xs)
  xs isa String && return xs |> split |> rmLayer
  xs = filter(x -> !((x isa String && all(isspace, x)) || isempty(x)), xs)
  xs = xs isa Array && length(xs) == 1 ? deNest(xs[1]) : xs
  xs = xs isa Array ? map(deNest, xs) : xs
  return xs
end

function if_(wast)
  If(body(wast[2][2:end]), length(wast) == 3 ? body(wast[3][2:end]) : [])
end

function op(wast)
  op = wast isa String ? wast : wast[1]
  dotop = split(op, ".")
  if length(dotop) == 2
    wtype = parse(WType, dotop[1])
    if dotop[2] == "const"
      return Const(wtype, parse(jltype(wtype), wast[2]))
      #TODO: Conversions, Truncations, e.t.c. Add as required.
    else
      return Op(wtype, Symbol(dotop[2]))
    end
  end
  op == "nop"         && return Nop()
  op == "select"      && return Select()
  op == "return"      && return Return()
  op == "unreachable" && return Unreachable()
  op == "get_local"   && return Local(parse(Int64, wast[2]))
  op == "set_local"   && return SetLocal(false, parse(Int64, wast[2]))
  op == "tee_local"   && return SetLocal(true, parse(Int64, wast[2]))
  op == "call"        && return Call(Symbol(wast[2][2:end]))
  op == "br"          && return Branch(false, parse(Int64, wast[2]))
  op == "br_if"       && return Branch(true, parse(Int64, wast[2]))
  op == "if"          && return if_(wast)
  op == "block"       && return block(wast[2:end])
  op == "loop"        && return loop(wast[2:end])

  error("Operator " * string(op) * " not defined.")
end

block(wast) = wast |> body |> Block
loop(wast)  = wast |> body |> Loop
body(wast) = map(op, wast)

function registers(wast, i, reg_type)
  rs = Vector{WType}()
  while wast[i][1] == reg_type
    push!(rs, parse(WType, wast[i][2]))
    i = i + 1
  end
  return rs, i
end

function func(wast)
  name = Symbol(wast[1][2][2:end])

  i = 2
  params,  i = registers(wast, i, "param")
  returns, i = registers(wast, i, "result")
  locals,  i = registers(wast, i, "local")

  bloc = block(wast[i:end])

  return Func(name, params, returns, locals, bloc)
end

function export_(wast)
  name = Symbol(wast[1][2][2:end-1])
  internalname = Symbol(wast[2][2][2:end])
  typ = Symbol(wast[2][1])
  return Export(name, internalname, typ)
end

function module_items(m, wast)
  p = wast[1] isa String ? wast[1] : wast[1][1]
  p == "func"   && push!(m.funcs,   func(wast))
  p == "export" && push!(m.exports, export_(wast))
  return m
end

function module_(wast)
  m = WebAssembly.Module([], [], [], [], [], [], [], Ref(0), [], [])
  return reduce(module_items, m, wast[2:end])
end
