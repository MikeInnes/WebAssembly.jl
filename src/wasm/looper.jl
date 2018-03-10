struct Label <: Instruction
  label::Int
end

struct Goto <: Instruction
  ifnot::Bool
  label::Int
end

Base.show(io::IO, i::Label) = print(io, "label ", i.label)
Base.show(io::IO, i::Goto) = print(io, i.ifnot ? "gotounless " : "goto ", i.label)
