function globalids(sig, slots)
  used = Set{Int}()
  ret = Int[]
  for T in sig
    i = findfirst(i -> slots[i] == T && !(i in used), 1:length(slots))
    i == nothing && (i = length(push!(slots, T)))
    push!(used, i)
    push!(ret, i-1)
  end
  return ret
end

function multivalue!(mod, f, xs, slots)
  for (i, x) in enumerate(xs)
    if x isa Return && length(f.result) > 1
      splice!(xs, i:0, reverse(SetGlobal.(globalids(f.result, slots))))
      return
    elseif x isa Call
      rs = func(mod, x.name).result
      length(rs) > 1 || continue
      splice!(xs, i+1:0, GetGlobal.(globalids(rs, slots)))
    elseif x isa Union{Block,If,Loop}
      multivalue_shim!(mod, f, x, slots)
    end
  end
end

multivalue_shim!(mod, f, c::Block, slots) = multivalue!(mod, f, c.body, slots)
multivalue_shim!(mod, f, c::Loop, slots)  = multivalue!(mod, f, c.body, slots)

multivalue_shim!(mod, f, c::If, slots) =
  (multivalue!(mod, f, c.t, slots); multivalue!(mod, f, c.t, slots))

function multivalue_shim!(mod::Module)
  slots = WType[]
  for f in mod.funcs
    multivalue_shim!(mod, f, f.body, slots)
  end
  for f in mod.funcs
    length(f.result) > 1 && empty!(f.result)
  end
  append!(mod.globals, Global.(slots))
  return mod
end
