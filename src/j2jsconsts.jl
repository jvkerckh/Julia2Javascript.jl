const StrSymb = Union{AbstractString, Symbol}

OUTPUTS = Dict(
  :console => "console.log",
  :alert => "window.alert",
  :document => "document.write",
  :doc => "document.write"
)

OPERATORS = [:+, :-, :*, :/, :^, :%, :inc, :dec, :(==), :(===), :!=, :!==, :>, :<, :>=, :<=, :!, :&, :|, :~, :⊻, :<<, :>>, :>>>]

ASSIGN_OPERATORS = [:(=), :+=, :-=, :*=, :/=, :^=, :%=, :<<=, :>>=, :>>>=, :&=, :|=, :⊻=]

LOGIC_OPERATORS = [:&&, :||]

MATHFUNCS = [:round, :ceil, :floor, :trunc, :sqrt, :abs, :sin, :cos, :min, :max, :log, :log2, :log10, :abs, :acos, :acosh, :asin, :asinh, :atan, :atanh, :cbrt, :cosh, :sign, :sqrt, :tan, :tanh]
