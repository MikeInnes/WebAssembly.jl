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
