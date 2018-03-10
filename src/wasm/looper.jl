struct Label <: Instruction
  label::Int
end

struct Goto <: Instruction
  ifnot::Bool
  label::Int
end

Base.show(io::IO, i::Label) = print(io, "label \$", i.label)
Base.show(io::IO, i::Goto) = print(io, i.ifnot ? "gotounless \$" : "goto \$", i.label)

function striplabels(is)
  ls = Dict{Int,Int}()
  js = []
  for (i, x) in enumerate(is)
    if x isa Label
      ls[x.label] = i
    else
      push!(js, x)
    end
  end
  for (i, x) in enumerate(js)
    x isa Goto && (js[i] = Goto(x.ifnot, ls[x.label]))
  end
  return js
end

function jumps(is)
  js = Pair{Int,Int}[]
  for (i, x) in enumerate(is)
    x isa Goto || continue
    push!(js, i => x.label)
  end
  return js
end

isforw(j) = j.first < j.second

contain(j, i::Integer) = minimum(j) <= i <= maximum(j)
contain(j1, j2) = contain(j1, j2.first) && contain(j1, j2.second)

jlength(j) = abs(j.first-j.second)

intersects(j1, j2) =
  jlength(j1) < jlength(j2) ? intersects(j2, j1) :
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
  return jumps
end
