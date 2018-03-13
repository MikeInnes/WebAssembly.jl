struct Label <: Instruction
  label::Int
end

struct Goto <: Instruction
  cond::Bool
  label::Int
end

Goto(label::Integer) = Goto(false, label)

Base.show(io::IO, i::Label) = print(io, "label \$", i.label)
Base.show(io::IO, i::Goto) = print(io, i.cond ? "gotounless \$" : "goto \$", i.label)

# Instead of arbitrary labels, jump to line numbers.
function striplabels(is)
  ls = Dict{Int,Int}()
  js = []
  for i in is
    if i isa Label
      ls[i.label] = length(js)+1
    else
      push!(js, i)
    end
  end
  for (i, x) in enumerate(js)
    x isa Goto && (js[i] = Goto(x.cond, ls[x.label]))
  end
  return js
end

function alignlabel(j, ls)
  from, to = j
  from < to && (from = ls[findlast(l -> l <= from, ls)])
  from=>to
end

# All jumps as pairs source_line => target_line
function jumps(is)
  js = Pair{Int,Int}[]
  for (i, x) in enumerate(is)
    x isa Goto || continue
    push!(js, i => x.label)
  end
  labels = sort(unique([1, map(x -> x.second, js)...]))
  return [alignlabel(j, labels) for j in js]
end

isforw(j) = j.first < j.second
jrange(j) = isforw(j) ? (j.first:j.second-1) : (j.second:j.first)

contain(j, i::Integer) = minimum(j) <= i <= maximum(j)
contain(j1, j2) = contain(j1, j2.first) && contain(j1, j2.second)

intersects(j1, j2) =
  length(jrange(j1)) < length(jrange(j2)) ? intersects(j2, j1) :
  contain(j1, j2.first) != contain(j1, j2.second)

order(j1, j2) = maximum(j1) > maximum(j2) ? (true, (j2, j1)) : (false, (j1, j2))

function checkintersect(jumps)
  for i = 1:length(jumps), j = i+1:length(jumps)
    @assert !intersects(jumps[i], jumps[j])
  end
end

function resolve!(jumps)
  for i = 1:length(jumps), j = i+1:length(jumps)
    intersects(jumps[i], jumps[j]) || continue
    switch, (j1, j2) = order(jumps[i], jumps[j])
    if isforw(j2)
      j2 = minimum(j1)=>j2.second
    else
      error("Can't resolve jumps $j1 and $j2")
    end
    jumps[i], jumps[j] = switch ? (j2, j1) : (j1, j2)
  end
  checkintersect(jumps)
  return sort!(jumps, lt = contain)
end

splice(is, r, js) = [is[1:r.start-1]..., js..., is[r.stop+1:end]...]

branches(x, label, level) = x

branches(x::Goto, label, level) =
  x.label == label ? Branch(x.cond, level) : x

branches(x::Block, label, level) = Block(branches.(x.body, label, level+1))
branches(x::Loop, label, level)  = Loop(branches.(x.body, label, level+1))

function insertjump(is, j)
  body = is[jrange(j)]
  body = branches.(body, j.second, 0)
  body = isforw(j) ? Block(body) : Block([Loop([body..., Branch(1)])])
  splice(is, jrange(j), [body, [nop for _ = 1:length(jrange(j))-1]...])
end

insertjumps(is, js) = reduce(insertjump, is, reverse(js))

function restructure(b::Block)
  is = striplabels(b.body)
  js = is |> jumps |> resolve!
  insertjumps(is, js) |> Block |> nops
end
