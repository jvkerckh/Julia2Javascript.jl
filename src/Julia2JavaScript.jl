module Julia2JavaScript

include("j2jsconsts.jl")
include("jsasync.jl")
include("jsdom.jl")
include("ajax.jl")


export @jscript

macro jscript( expr, isterm::Bool=true )
  expstr = processexpr( expr, isterm )
  
  # For code blocks
  if expr isa Expr && expr.head === :block && expstr isa Vector && @inbounds expstr[1] == "{" && @inbounds expstr[end] == "}"
    expstr = map( str -> (@inbounds str[3:end]), @inbounds expstr[2:end-1] )
  end

  expstr isa Vector ? join( expstr, '\n') : expstr
end


ts(isterm::Bool) = isterm ? ';' : ""

processexpr( num::Number, isterm::Bool=false ) =
  string( num, isterm |> ts )
processexpr( num::Unsigned, isterm::Bool=false ) =
  string( "0x", string( num, base=16 ) |> uppercase, isterm |> ts )
processexpr( str::AbstractString, isterm::Bool=false ) =
  string( "'", str, "'", isterm |> ts )
processexpr( chr::Char, isterm::Bool=false ) = processexpr( chr |> string, isterm )
processexpr( num::BigInt, isterm::Bool=false ) =
  string( num, 'n', isterm |> ts )
processexpr( ::Irrational{:π}, isterm::Bool=false ) =
  string( "Math.PI", isterm |> ts )
processexpr( ::Irrational{:ℯ}, isterm::Bool=false ) =
  string( "Math.E", isterm |> ts )
processexpr( ::Nothing, isterm::Bool=false ) = string( "null", isterm |> ts )
processexpr( ::Missing, isterm::Bool=false ) =
  string( "undefined", isterm |> ts )

function processexpr( vname::Symbol, isterm::Bool=false )
  vname === :nothing && return processexpr( nothing, isterm )
  vname === :missing && return processexpr( missing, isterm )
  string( vname, isterm |> ts )
end

processexpr( vname::QuoteNode, isterm::Bool=false ) = processexpr( vname.value, isterm )


function processexpr( expr, isterm::Bool=false )
  expstr = procexpr(expr)
  isnothing(expstr) && return
  @inbounds expstr[end] == "}" && (isterm = false)
  # expr.head ∈ [:block, :function, :if, :elseif, :for] && (isterm = false)

  isterm || return expstr
  expstr isa String && return string( expstr, ';' )
  @inbounds expstr[end] = string( expstr[end], ';' )
  expstr
end


function processrecipe( recipe, args )
  @inbounds arglist = recipe[2:end]
  numargs = filter( arg -> arg isa Signed, arglist )
  rind = isempty(numargs) ? 0 : max( 0, filter( arg -> arg isa Signed, arglist ) |> maximum ) + 1
  @inbounds arglist = vcat( map( arglist ) do aind
    aind isa Signed || return aind
    aind > 0 && return @inbounds args[aind]
    @inbounds arg = args[rind:end]
    aind == -1 && return arg
    [arg]
  end... )
  recipe[1](arglist...)
end


function procexpr( expr::Expr )
  haskey( JEXPRS, expr.head ) && return processrecipe( JEXPRS[expr.head], expr.args )

  println( expr, "   (", typeof(expr), ")" )
  display( expr.head )
  println( expr.args, "   (", typeof(expr.args), ")" )
end


function processfuncall( fcall, fargs::Vector )
  haskey( FCALLS, fcall ) && return processrecipe( FCALLS[fcall], fargs )
  # length must be treat separately because of the difference in syntax between Julia and JavaScript.
  fcall === :length && return @inbounds processexpr( :($(fargs[1]).length) )
  
  processfcall( fcall, fargs... )
end


function processmacro( fcall, fargs::Vector )
  fcall isa GlobalRef && @inbounds return processcmd(fargs[1])
  haskey( MACROCALLS, fcall ) && return processrecipe( MACROCALLS[fcall], fargs )

  processfcall( fcall, fargs... )
end


function processcall( fcall, fargs::Vector, ismacro::Bool )
  filter!( arg -> !(arg isa LineNumberNode), fargs )
  ismacro ? processmacro( fcall, fargs ) : processfuncall( fcall, fargs )
end


function processoutput( outtype::Symbol, args... )
  haskey( OUTPUTS, outtype ) || error( ArgumentError( "Output type must be one of :console, :alert, :document, :doc" ) )
  string( OUTPUTS[outtype], '(', join( processexpr.(args), ", " ), ')' )
end

processoutput( outtype::QuoteNode, args... ) =
  processoutput( outtype.value, args... )


function processdict( entries::Vector )
  isempty(entries) && return "{}"

  entstr = map( processexpr.(entries) ) do estr
    estr isa String && return string( estr, ',' )
    @inbounds estr[end] = string( estr[end], ',' )
    estr
  end

  entstr = vcat( entstr... )
  @inbounds entstr[end] = entstr[end][1:end-1]
  vcat( "{", string.( "  ", entstr ), "}" )
end


function processpair( first::AbstractString, second )
  secstr = processexpr(second)
  secstr isa Vector || return string( first, ": ", secstr )
  @inbounds secstr[1] = string( first, ": ", secstr[1] )
  secstr
end

processpair( first::QuoteNode, second ) = processpair( first.value, second )
processpair( first::Symbol, second ) = processpair( first |> string, second )


function processassign( aop::Symbol, vname, expr )
  # This catches stuff like x = 5, 6
  vname isa Symbol && expr isa Expr && expr.head === :tuple && (expr.head = :vect)

  # For stuff like x, y = 5, 6
  if vname isa Expr && vname.head === :tuple
    nvars = min( length(vname.args), expr isa Expr && expr.head === :tuple ? length(expr.args) : 1 )
    @inbounds expstrs = processassign.( aop, vname.args[1:nvars], expr isa Expr ? expr.args[1:nvars] : expr )
    
    for ii in 1:nvars-1
      if @inbounds expstrs[ii] isa String
        @inbounds expstrs[ii] = string( expstrs[ii], ',' )
      else
        @inbounds expstrs[ii][end] = string( expstrs[ii][end], ',' )
      end
    end

    expstrs = vcat(expstrs...)
    @inbounds expstrs[2:end] = string.( "  ", expstrs[2:end] )
    return expstrs
  end

  # Regular assignment
  aopstr = aop === :^= ? " **= " : " $aop "
  expstr = processexpr(expr)
  nexpr = processexpr(vname)
  expstr isa String && return string( nexpr, aopstr, expstr )
  @inbounds expstr[1] = string( nexpr, aopstr, expstr[1] )
  expstr
end
  

function processoperator( op::Symbol, args::Vector )
  op ∈ [:inc, :dec] && return processinc( op, args... )
  length(args) == 1 && return string( op, processexpr(args[1]) )

  if op === :^
    opstr = " ** "
  elseif op === :⊻
    opstr = " ^ "
  else
    opstr = " $op "
  end

  join( processargs(args), opstr )
end


function processinc( incsym::Symbol, vname::Symbol, preinc::Bool=false )
  incstr = incsym === :inc ? "++" : "--"
  preinc ? "$incstr$vname" : "$vname$incstr"
end

function processinc( incsym::Symbol, vexpr::Expr, preinc::Bool=false )
  incstr = incsym === :inc ? "++" : "--"
  vexpstr = processexpr(vexpr)
  preinc ? "$incstr($vexpstr)" : "($vexpstr)$incstr"
end


function processarg( arg )
  argstr = processexpr(arg)
  arg isa Expr && (argstr = "($argstr)")
  argstr
end


processargs( args::Vector ) = processarg.(args)


function processdec( dectype::Symbol, decs... )
  isempty(decs) && error( ArgumentError( "At least one declaration must be made" ) )
  validatedecs( decs |> collect, dectype === Symbol("@const") ) || error( ArgumentError( "All declarations must be in the form of a Symbol (variable), or a keyword-value pair, with only the latter permitted for const declarations." ) )
  prestr = string(dectype)[2:end]
  ndecs = length(decs)
  decstr = processexpr.( decs |> collect )

  map( eachindex(decstr) ) do ii
    sufstr = ii == ndecs ? "" : ","
    @inbounds decstr[ii] isa String && return @inbounds decstr[ii] = string( decstr[ii], sufstr )
    @inbounds decstr[ii][end] = string( decstr[ii][end], sufstr )
  end

  decstr = vcat(decstr...)

  map( eachindex(decstr) ) do ii
    pstr = ii == 1 ? prestr : " "
    @inbounds decstr[ii] = string( pstr, " ", decstr[ii] )
  end

  join( decstr, '\n' )
end


function validatedecs( decs, isconst )
  map( decs ) do vdec
    !isconst && vdec isa Symbol && return true
    vdec isa Expr || return false
    vdec.head === :(=)
  end |> all
end


processkwarg( vname::Symbol, expr ) = string( vname, " = ", processexpr(expr) )


function processifelse( cond, ifexpr, elexpr=nothing )
  condstr = processexpr(cond)
  exprstr = processexpr(ifexpr)
  elstr = ""
  isnothing(elexpr) || (elstr = processexpr(elexpr) )

  if !(isblockexpr(ifexpr) || isblockexpr(elexpr))
    @inbounds condll = condstr isa String ? "($condstr) ? (" : string( condstr[end], ") ? (" )
    @inbounds elfl = elstr isa String ? ") : ($elstr)" : string( ") : (", elstr[1] )

    if exprstr isa String
      exprstr = "$condll$exprstr$elfl"
    else
      @inbounds exprstr[1] = string( condll, exprstr[1] )
      @inbounds exprstr[end] = string( exprstr[end], elfl )
    end

    if condstr isa Vector
      @inbounds condstr[1] = string( "(", condstr[1] )
      @inbounds exprstr = vcat( condstr[1:end-1], exprstr )
    end

    if elstr isa Vector
      @inbounds elstr[end] = string( elstr[end], ")" )
      @inbounds append!( exprstr, elstr[2:end] )
    end

    return exprstr
  end

  condstr isa String || (@inbounds condstr = strip(condstr[2][1:end-1]))
  @inbounds exprstr[1] = string( "if (", condstr, ") {" )
  isempty(elstr) && return exprstr
  @inbounds elstr[1] = string( "} else ", elstr[1] )
  @inbounds vcat( exprstr[1:end-1], elstr )
end


isblockexpr(expr) = expr isa Expr && expr.head === :block


processtypeof( expr ) = string( "typeof ", processarg(expr) )


function processfunction( fcall::Expr, fbody )
  fcallstr = processfname(fcall)
  fbodystr = processexpr(fbody)
  @inbounds fbodystr[1] = string( fcallstr, " {" )
  fbodystr
end


function processfname( fcall::Expr )
  fstr = processexpr.(fcall.args)
  fargs = @inbounds join( fcall.head === :call ? fstr[2:end] : fstr, ", " )
  @inbounds string( "function ", fcall.head === :call ? fstr[1] : "", "(", fargs, ")")
end


function processblock( blines::Vector )
  lstr = processexpr.( filter( bline -> !(bline isa LineNumberNode), blines ), true )
  expstr = vcat( "{", string.( "  ", vcat(lstr...) ), "}" )
  expstr
end


function processreturn( expr )
  isnothing(expr) && return "return"
  rexpr = processexpr(expr)
  rexpr isa Vector || return string( "return ", rexpr )
  @inbounds rexpr[1] = string( "return ", rexpr[1] )
  rexpr
end


processpoint( vname ) = processexpr(string(vname))

function processpoint( vname, comp )
  vexpr = processexpr(vname)
  cexpr = processexpr(comp)
  vexpr isa String && return string( vexpr, '.', cexpr )
  @inbounds vexpr[end] = string( vexpr[end], '.', cexpr )
  vexpr
end


processref( vname, ind::Integer ) =
  string( processexpr(vname), '[', ind-1, ']' )
  
function processref( vname, ind )
  vexp = processexpr(vname)
  indexp = ind === :end ? string( vexp, ".length - 1" ) : processexpr(ind)
  string( vexp, '[', indexp, ']' )
end


function processfcall( fname, args... )
  fstr = processexpr(fname)
  fargstr = processexpr.(args |> collect)

  if all( isa.( fargstr, AbstractString) )
    fargstr = string( '(', join( fargstr, ", " ), ')' )
    fstr isa String && return string( fstr, fargstr )
    @inbounds fstr[end] = string( fstr[end], fargstr )
    return fstr
  end

  for (ii, val) in enumerate(fargstr)
    if fargstr[ii] isa AbstractString
      @inbounds fargstr[ii] = string( val, ',' )
    else
      @inbounds fargstr[ii][end] = string( fargstr[ii][end], ',' )
    end
  end

  fargstr = string.( "  ", vcat(fargstr...) )
  @inbounds fargstr[end] = fargstr[end][1:end-1]
  fstr isa String && return vcat( string( fstr, '(' ), fargstr, ')' )
  @inbounds fstr[end] = string( fstr[end], '(' )
  vcat( fstr, fargstr, ')' )
end


processtemplate( arg::AbstractString ) = string( '`', arg, '`' )
processtemplate( arg::Expr ) = string( '`', join(processexpr(arg)), '`')


function processstring( args::Vector )
  argstr = map( args ) do arg
    arg isa AbstractString ? arg : string( "\${", processexpr(arg), "}")
  end
end


function processnew( arg::Expr )
  argstr = processexpr(arg)
  argstr isa String && return "new $argstr"
  @inbounds argstr[1] = string( "new ", argstr[1] )
  argstr
end


function processvector( vect::Vector )
  exprs = processexpr.(vect)
  all(isa.( exprs, String )) && return string( '[', join( processexpr.(vect), ", " ), ']' )
  vlen = length(vect)

  map( eachindex(exprs) ) do ii
    ii == vlen && return
    @inbounds expr = exprs[ii]

    if expr isa String
      exprs[ii] = string( exprs, ',' )
      return
    end

    @inbounds expr[end] = string( expr[end], ',' )
  end

  exprs = vcat(exprs...)
  vcat( "[", string.( "  ", exprs ), "]" )
end


processsort( arg ) = string( processexpr(arg), ".sort()" )


processforeach( vexpr, fexpr ) =
  string( processexpr(vexpr), ".forEach(", processexpr(fexpr), ")" )


processpush( vexpr, expr ) =
  string( processexpr(vexpr), ".push(", processexpr(expr), ")" )


processrand() = "Math.random()"

function processrand( expr::Expr )
  dstr = string( "Math.random(", processexpr(expr), ")")
  expr.head === :call || return dstr
  3 <= length(expr.args) <= 4 || return dstr
  @inbounds expr.args[1] === :(:) || return dstr
  @inbounds sval = expr.args[2]
  @inbounds range = abs(expr.args[end] - sval)
  skip = length(expr.args) == 3 ? 1 : expr.args[3]
  nvals = floor( Int, range / abs(skip) ) + 1
  string( "Math.floor(Math.random() * $nvals)", skip == 1 ? "" : " * $skip",  iszero(sval) ? "" : " + $sval" )
end


function processfor( rvar, expr )
  rexpr = processforrange(rvar)
  expstr = processexpr(expr)

  if rexpr isa String
    @inbounds expstr[1] = string( "for ", rexpr, " {" )
    return expstr
  end

  if rexpr isa Tuple
    return @inbounds vcat(
      string( "for ", rexpr[1], " {" ),
      rexpr[2],
      expstr[2:end]
    )
  end

  prestr = map( eachindex(rexpr) ) do ii
    @inbounds string( repeat( "  ", ii-1 ), "for ", rexpr[ii], " {" )      
  end
  sufstr = map( ii -> string( repeat( "  ", ii-1 ), '}' ), reverse(eachindex(rexpr)) )
  wsp = repeat( "  ", length(rexpr)-1 )
  return @inbounds vcat(
    prestr,
    string.( wsp, expstr[2:end-1] ),
    sufstr
  )
end


function processforrange( rvar )
  rvar.head == :block && return processforrange.(rvar.args)
  
  lvar = @inbounds rvar.args[1]
  rng = @inbounds rvar.args[2]

  lvar isa Expr && rng isa Symbol && return processforrange( lvar.args..., rng )

  if lvar isa Symbol && rng isa Symbol
    rexpr = string( "let ", lvar, " of ", rng )
  elseif rng.head === :call && @inbounds rng.args[1] === :(:)
    rexpr = processforrange( rng.args, lvar )
  elseif rng.head === :call && @inbounds rng.args[1] === :eachindex
    rexpr = @inbounds processforrange( :(0:length($(rng.args[2]))).args, lvar )
  elseif rng.head === :call && @inbounds rng.args[1] === :keys
    @inbounds rexpr = string( "let ", lvar, " in ", rng.args[2] )
  else
    rexpr = string( "let ", lvar, " of ", rng )
  end

  "($rexpr)"
end


function processforrange( rng::Vector, lvar::Symbol )
  @inbounds svar, evar = rng[2], rng[end]
  @inbounds skip = length(rng) == 3 ? 1 : rng[3]
  sexpr = skip == 1 ? :(inc($lvar)) : (skip == -1 ? :(dec($lvar)) : :($lvar += $skip) )
  rexpr = [
    processexpr(:(@let $lvar = $svar))
    processexpr( evar isa Number ? :($lvar <= $evar) : :($lvar < $evar))
    processexpr(sexpr)
  ]
  rexpr = join( rexpr, "; " )
end


processforrange( kvar::Symbol, vvar::Symbol, rng::Symbol ) =
  "(let $kvar in $rng)", "  $vvar = $rng[$kvar];"


function processwhile( cexpr, rexpr::Expr )
  cstr = processexpr(cexpr)
  expstr = processexpr(rexpr)
  @inbounds expstr[1] = string( "while (", cstr, ") {" )
  expstr
end


processregex( ptrn::String, flags::String="" ) = "/$ptrn/$flags"


function processtry( texpr::Expr, evar, cexpr )
  tstr = processexpr(texpr)
  @inbounds tstr[1] = "try {"

  if cexpr === false
    cstr = String[]
  else
    cstr = cexpr === false ? String[] : processexpr(cexpr)
    @inbounds cstr[1] = string( "catch", evar isa Symbol ? "($evar)" : "", " {" )
  end

  vcat( tstr, cstr )
end


function processtry( texpr::Expr, evar, cexpr, fexpr::Expr )
  tcstr = processtry( texpr, evar, cexpr )
  fstr = processexpr(fexpr)
  @inbounds fstr[1] = "finally {"
  vcat( tcstr, fstr )
end


function processthrow( expr )
  expstr = processexpr(expr)
  @inbounds expstr[1] = string( "throw ", expstr[1] )
  expstr
end


function processanonfunction( fvars, fbody )
  vstr = processexpr(fvars)
  bstr = processexpr(fbody)

  if count( expr -> !(expr isa LineNumberNode), fbody.args ) == 1
    @inbounds bstr[2] = string( vstr, " => ", bstr[2][3:end] )
    @inbounds bstr[end-1][end] == ';' && (bstr[end-1] = bstr[end-1][1:end-1])
    @inbounds return length(bstr) == 3 ? bstr[2] : bstr[2:end-1]
  end

  @inbounds bstr[1] = string( vstr, " => {" )
  bstr
end


function processtuple( texprs::Vector )
  tstr = processexpr.(texprs)
  string( '(', join( tstr, ", " ), ')' )
end


function processrange( sval::Number, step::Number, eval::Number )
  rvals = (sval:step:eval) |> collect
  expr = Expr( :vect )
  expr.args = rvals
  processexpr(expr)
end

processrange( sval::Number, eval::Number ) = processrange( sval, 1, eval )
processrange( sval, step, eval ) = processfcall( :(:), sval, step, eval )
processrange( sval, eval ) = processfcall( :(:), sval, eval )


function processdollar( expr )
  expstr = processexpr(expr)
  expstr isa String && return string( '$', expstr )
  @inbounds expstr[1] = string( '$', expstr[1] )
  expstr
end


processcmd( expr ) = "`$expr`"


function processexport( exprs::Vector, juliaexport::Bool )
  juliaexport &&
    return string( "export { ", join( processexpr.(exprs), ", " ), " }" )

  @assert all( exprs ) do expr
    expr isa Symbol && return true
    expr isa Expr && expr.head === :macrocall && (@inbounds expr.args[1] === Symbol("@as"))
  end "Variable arguments must be of type Symbol or macrocalls with @as, @default, or @namespace"
  
  string( "export { ", join( processexpr.(exprs), ", " ), " }" )
end

function processexport( isdefault::Bool, expr )
  expstr = processexpr(expr)
  prefix = string( "export ", isdefault ? "default " : "" )
  expstr isa Vector || return "$prefix$expstr"
  @inbounds expstr[1] = string( prefix, expstr[1] )
  expstr
end


processimport( expr ) = string( "import ", processexpr(expr), ";" )

function processimport( expr::Vector )
  expstr = processimport.(expr)
  @inbounds expstr[end] = expstr[end][1:end-1]
  expstr
end

function processimport( mexpr, exprs )
  @assert all( exprs ) do expr
    expr isa Symbol && return true
    expr isa Expr && expr.head === :macrocall && (@inbounds expr.args[1] ∈ VALID_IMPORTS)
  end "Variable arguments must be of type Symbol or macrocalls with @as, @default, or @namespace"

  mstr = processexpr(string(mexpr))
  isempty(exprs) && return "import $mstr"

  # Group all variable imports
  issymb = map( exprs ) do expr
    expr isa Symbol || (@inbounds expr.args[1] === Symbol("@as"))
  end

  sinds = findall(issymb)
  nsinds = findall(.!issymb)

  # Generate JS expression
  expstr = processexpr.(exprs)
  @inbounds sexpstr = isempty(sinds) ? "" : string( "{ ", join( expstr[sinds], ", " ), " }" )
  @inbounds nsexpstr = join( expstr[nsinds], ", " )
  string( "import ", join( [nsexpstr, sexpstr], isempty(nsexpstr) || isempty(sexpstr) ? "" : ", " ), " from ", mstr )
end


processcolon( mexpr, vexpr ) = string( "{ ", join( processexpr.(vexpr), ", " ), " } from ", processexpr(mexpr) )


processimparg( expr ) = processexpr(expr)
processimparg( ::AbstractString, expr ) = string( "* as ", processexpr(expr) )
processimparg( vexpr, nexpr ) = string( processexpr(vexpr), " as ", processexpr(nexpr) )


include("jsrecipes.jl")

end
