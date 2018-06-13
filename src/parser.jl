function parsewast(filename)
  f = open(filename)
  s = readstring(f)
  close(f)

  brackets = parsebrackets(s)

end

parsebrackets(s) = pb(s, 1)

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

function op(wast)
  args = split(wast)
  op = split(args[1], ".")
  if length(op) == 2
    wtype = parse(WType, op[1])
    if op[2] == "const"
      return Const(wtype, parse(jltype(wtype), args[2]))
    # else if op[2] == "convert" # Need to handle all the conversions at some point.
    else
      return Op(wtype, Symbol(op[2]))
    end
  end
  op = split(args[1])
  op[1] == "nop"         && return Nop()
  op[1] == "get_local"   && return Local(parse(Int64, args[2]))
  op[1] == "set_local"   && return SetLocal(false, parse(Int64, args[2]))
  op[1] == "tee_local"   && return SetLocal(true, parse(Int64, args[2]))
  op[1] == "call"        && return Call(Symbol(args[2][2:end]))
  op[1] == "select"      && return Select()
  op[1] == "return"      && return Return()
  op[1] == "unreachable" && return Unreachable()

  #TODO: Ifs, Conversions, blocks. others.

end

function block(wast)
  return Block(map(op, wast))
end

#todo, named registers?
function registers(wast, i, n)
  rs = Vector{WType}()
  while wast[i][1] == n[1]
    r = split(wast[i])
    if r[1] == n
      push!(rs, parse(WType, r[2]))
    end
    i = i + 1
  end
  return (rs, i)
end

function func(wast)

  #Name
  name = Symbol(split(wast[1])[2][2:end])

  #Parameters
  wast = filter(x->typeof(x) == Array{Any, 1}, wast)
  wast = map(x->x[1], wast)
  i = 1
  (params, i) = registers(wast, i, "param")
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
