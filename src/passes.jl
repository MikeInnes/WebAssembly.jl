applyblock(f, x::Union{Block,Loop}) = typeof(x)(f(x.body))
applyblock(f, x::If) = If(f(x.t), f(x.f))
applyblock(f, x) = x

walk(x, inner, outer) = outer(x)
walk(x::Union{Block,Loop,If}, inner, outer) = outer(applyblock(xs -> map(inner, xs), x))

postwalk(f, x) = walk(x, x -> postwalk(f, x), f)
prewalk(f, x)  = walk(f(x), x -> prewalk(f, x), identity)

# remove nops

nops(x) = prewalk(x -> applyblock(is -> filter(i -> i ≠ nop, is), x), x)

# dead code

function deadcode(x)
  prewalk(x) do x
    applyblock(x) do is
      i = findfirst(b -> b isa Branch && !b.cond, is)
      i == 0 ? is : is[1:i]
    end
  end
end

# recover ifs

function makeifs(code)
  prewalk(code) do x
    x isa Block || return x
    i = findfirst(x -> x isa Branch, x.body)
    i != 0 && x.body[i].cond || return x
    cond = x.body[1:i-1]
    cond[end] == Op(i32, :eqz) ? pop!(cond) : push!(cond, Op(i32, :eqz))
    return Block([cond..., If(x.body[i+1:end], [])])
  end
end

# remove unused blocks
# TODO: collapse blocks with the same head

branches_to(b, l) = false
branches_to(b::Branch, l) = b.level == l

branches_to(b::Union{Block,Loop}, l) = any(x -> branches_to(x, l+1), b.body)

branches_to(b::If, l) =
  any(x -> branches_to(x, l+1), b.t) || any(x -> branches_to(x, l+1), b.f)

isredundant(b::Block) = !branches_to(b, -1)

function rmblocks(code)
  prewalk(code) do x
    applyblock(x) do is
      is′ = []
      for i in is
        i isa Block && isredundant(i) ? append!(is′, i.body) : push!(is′, i)
      end
      is′
    end
  end
end
