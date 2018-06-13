function parsewast(filename)
  f = open(filename)
  s = readstring(f)
  close(f)

  # brackets = parsebrackets(s)
  # @show brackets[1][1]
  # @show (s |> parsebrackets |> deNest)
  (s |> parsebrackets |> deNest) |> func
end

parsebrackets(s) = pb(s, 1)[1]

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
    # else if op[2] == "convert" # Need to handle all the conversions at some point.
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

  #TODO: Ifs, Conversions, blocks. others.
  error("Operator " * string(op) * " not defined.")

end

block(wast) = Block(body(wast))
loop(wast) = Loop(body(wast))
body(wast) = map(op, wast)

rmLayer(xs::Array) = length(xs) == 1 ? xs[1] : xs

# Split strings, remove white space
function deNest(xs)
  @show xs
  xs isa String && return rmLayer(split(xs))
  xs = filter(x -> !((x isa String && all(isspace, x)) || (x isa Array && length(x) == 0)), xs)
  xs = xs isa Array && length(xs) == 1 ? deNest(xs[1]) : xs
  xs = xs isa Array ? map(x -> deNest(x), xs) : xs
end


#todo, named registers?
function registers(wast, i, n)
  @show wast[i]
  @show wast[i][1]
  rs = Vector{WType}()
  while wast[i][1] == n
    push!(rs, parse(WType, wast[i][2]))
    i = i + 1
  end
  return (rs, i)
end

function func(wast)
  # println("New Run")

  @show wast
  #Name
  name = Symbol(wast[1][2:end])

  #Parameters
  # wast = filter(s -> !(s isa String && all(isspace, s)), wast)
  # wast = map(x->x[1], wast)
  # wast = deNest(wast)
  i = 2
  (params, i) = registers(wast, i, "param")
  # @show params
  (returns, i) = registers(wast, i, "result")
  (locals, i) = registers(wast, i, "local")
  # while wast[i][2] != 'r'
  #   ps = split(wast[i])
  #   if ps[i] == "param"
  #     push!(params, parse(WType, ps[2]) :: WType)
  #   end
  #   i = i + 1
  # end

  #Returns
  # ps = split(wast[i])
  # returns = parse(WType, ps[2])

  #Ops
  bloc = block(wast[i:end])

  return Func(name, params, returns, locals, bloc)
end

# function pb(ss)
#   println(ss)
#   ns = split(ss[1], ")", limit = 2)
#   if length(ns) == 2
#     return ("", [ns[1]], vcat([ns[2]], ss[2:end]))
#   elseif length(ns[end]) > 0 && ns[end][end] == ")"
#     # return [[ns[1]], pb(ss[2:end])]
#     return ("", ns, ss[2:end])
#   else
#     if length(ss) > 1
#       ns = pb(ss[2:end])
#       # if length(ns) > 1
#       #   return vcat([ss[1], [ns[1]]], ns[2:end])
#       # else
#       #   return [ss[1], [ns[1]]]
#       # end
#       return (ns[1] + ss[1], )
#     else
#       return [ss[1]]
#     end
#   end
# end


# function parsefunc(s)
#   ss = split(s, "\n")
#   funcdef = rmbrackets(ss[1])
