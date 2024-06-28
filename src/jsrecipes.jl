# Recipes for expressions, based on expr.head
begin
  JEXPRS = Dict(
    :call => (processcall, 1, -2, false),
    :macrocall => (processcall, 1, -2, true),
    :const => (processdec, Symbol("@const"), -1),
    :kw => (processkwarg, -1),
    :if => (processifelse, -1),
    :elseif => (processifelse, -1),
    :function => (processfunction, -1),
    :(::) => (processexpr, 1),
    :block => (processblock, -2),
    :return => (processreturn, -1),
    :. => (processpoint, -1),
    :ref => (processref, -1),
    :string => (processstring, -2),
    :vect => (processvector, -2),
    :break => (string, :break),
    :continue => (string, :continue),
    :for => (processfor, -1),
    :while => (processwhile, -1),
    :try => (processtry, -1),
    :-> => (processanonfunction, -1),
    :tuple => (processtuple, -2),
    :$ => (processdollar, -1),
    :export => (processexport, -2, true),
    :import => (processimport, -2),
    :(:) => (processcolon, 1, -2),
  )

  for aop in ASSIGN_OPERATORS
    JEXPRS[aop] = (processassign, aop, -1)
  end

  for lop in LOGIC_OPERATORS
    JEXPRS[lop] = (processoperator, lop, -2 )
  end
end

# Recipes for specific function calls
begin
  FCALLS = Dict(
    :output => (processoutput, -1),
    :Dict => (processdict, -2),
    :(=>) => (processpair, -1),
    :string => (processoperator, :+, -2),
    :typeof => (processtypeof, -1),
    :sort => (processsort, -1),
    :foreach => (processforeach, 2, 1),
    :push! => (processpush, -1),
    :rand => (processrand, -1),
    :throw => (processthrow, -1),
    :|> => (processthen, -1),
    :(:) => (processrange, -1),
  )

  for op in OPERATORS
    FCALLS[op] = (processoperator, op, -2)
  end

  for func in MATHFUNCS
    FCALLS[func] = (processfcall, :(Math.$func), -1)
  end
end

# Recipes for specific macro calls
begin
  MACROCALLS = Dict{Symbol,Tuple}(
    Symbol("@output") => (processoutput, -1),
    Symbol("@new") => (processnew, -1),
    Symbol("@template") => (processtemplate, -1),
    Symbol("@r_str") => (processregex, -1),
    Symbol("@usestrict") => (() -> "'use strict'",),
    Symbol("@timeout") => (processtimeout, -1),
    Symbol("@promise") => (processpromise, -1),
    Symbol("@DOM") => (processdomcall, -1),
    Symbol("@async") => (processasync, -1),
    Symbol("@await") => (processawait, -1),
    Symbol("@AJAX") => (processajaxcall, -1),
    Symbol("@export") => (processexport, -2, false),
    Symbol("@dexport") => (processexport, 1, 2),
    Symbol("@import") => (processimport, 1, -2),
    Symbol("@default") => (processimparg, 1),
    Symbol("@namespace") => (processimparg, "*", 1),
    Symbol("@as") => (processimparg, 1, 2),
  )

  for dec in [Symbol("@var"), Symbol("@let"), Symbol("@const")]
    MACROCALLS[dec] = (processdec, dec, -1)
  end
end
