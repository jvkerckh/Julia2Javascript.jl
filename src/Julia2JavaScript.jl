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
processexpr( num::BigInt, isterm::Bool=false ) =
  string( num, 'n', isterm |> ts )
processexpr( ::Irrational{:π}, isterm::Bool=false ) =
  string( "Math.PI", isterm |> ts )
processexpr( ::Irrational{:ℯ}, isterm::Bool=false ) =
  string( "Math.E", isterm |> ts )
processexpr( ::Nothing, isterm::Bool=false ) = string( "null", isterm |> ts )
processexpr( ::Missing, isterm::Bool=false ) =
  string( "undefined", isterm |> ts )
processexpr( ::Nothing, isterm::Bool=false ) = string( "null", isterm |> ts )
processexpr( vname::Symbol, isterm::Bool=false ) = string( vname, isterm |> ts )
processexpr( vname::QuoteNode, isterm::Bool=false ) = processexpr( vname.value, isterm )


function processexpr( expr, isterm::Bool=false )
  expstr = procexpr(expr)
  expstr isa Nothing && return
  @inbounds expstr[end] == "}" && (isterm = false)
  # expr.head ∈ [:block, :function, :if, :elseif, :for] && (isterm = false)

  isterm || return expstr
  expstr isa String && return string( expstr, ';' )
  @inbounds expstr[end] = string( expstr[end], ';' )
  expstr
end


function procexpr( expr::Expr )
  expr.head ∈ [:call, :macrocall] && return @inbounds processcall( expr.args[1], expr.args[2:end] )
  expr.head ∈ ASSIGN_OPERATORS && return processassign( expr.head, expr.args... )
  expr.head === :const && return processdec( Symbol("@const"), expr.args... )
  expr.head === :kw && return processkwarg(expr.args...)
  expr.head ∈ [:if, :elseif] && return processifelse(expr.args...)
  expr.head ∈ LOGIC_OPERATORS && return processoperator( expr.head, expr.args )
  expr.head === :function && return processfunction(expr.args...)
  expr.head === :(::) && return @inbounds processexpr(expr.args[1])
  expr.head === :block && return processblock(expr.args)
  expr.head === :return && return @inbounds processreturn(expr.args[1])
  expr.head === :. && return processpoint(expr.args...)
  expr.head === :ref && return processref(expr.args...)
  expr.head === :string && return processstring(expr.args)
  expr.head === :vect && return processvector(expr.args)
  expr.head ∈ [:break, :continue] && return string(expr.head)
  expr.head === :for && return processfor(expr.args...)
  expr.head === :while && return processwhile(expr.args...)
  expr.head === :try && return processtry(expr.args...)
  expr.head === :-> && return processanonfunction(expr.args...)
  expr.head === :tuple && return processtuple(expr.args)
  
  println( expr, "   (", typeof(expr), ")" )
  display( expr.head )
  println( expr.args, "   (", typeof(expr.args), ")" )
end


function processcall( fcall, fargs::Vector )
  filter!( arg -> !(arg isa LineNumberNode), fargs )
  
  fcall === Symbol("@new") && return processnew(fargs...)
  fcall ∈ [Symbol("@var"), Symbol("@let"), Symbol("@const")] && return processdec( fcall, fargs... )
  fcall === Symbol("@template") && return processtemplate(fargs...)
  fcall === Symbol("@r_str") && return processregex(fargs...)
  fcall === Symbol("@usestrict") && return "'use strict'"
  fcall === Symbol("@timeout") && return processtimeout(fargs...)
  fcall === Symbol("@promise") && return processpromise(fargs...)
  fcall === Symbol("@DOM") && return processdomcall(fargs...)
  fcall === Symbol("@async") && return processasync(fargs...)
  fcall === Symbol("@await") && return processawait(fargs...)
  fcall === Symbol("@AJAX") && return processajaxcall(fargs...)

  fcall === :output && return processoutput(fargs...)
  fcall === :Dict && return processdict(fargs)
  fcall === :(=>) && return processpair(fargs...)
  fcall ∈ OPERATORS && return processoperator( fcall, fargs )
  fcall === :string && return processoperator( :+, fargs )
  fcall === :typeof && return processtypeof(fargs...)
  fcall === :length && return @inbounds processexpr( :($(fargs[1]).length) )
  fcall === :sort && return processsort(fargs...)
  fcall === :foreach && return @inbounds processforeach( fargs[2], fargs[1] )
  fcall === :push! && return @inbounds processpush( fargs[1], fargs[2] )
  fcall ∈ MATHFUNCS && return processfcall( :(Math.$fcall), fargs... )
  fcall === :rand && return processrand(fargs...)
  fcall === :throw && return processthrow(fargs...)
  fcall === :|> && return processthen(fargs...)
  fcall === :(:) && return processrange(fargs...)

  processfcall( fcall, fargs... )
end


function processoutput( outtype::QuoteNode, args... )
  haskey( OUTPUTS, outtype.value ) || error( ArgumentError( "Output type must be one of :console, :alert, :document, :doc" ) )
  string( OUTPUTS[outtype.value], '(', join( processexpr.(args), ", " ), ')' )
end


function processdict( entries::Vector )
  entstr = vcat( processexpr.(entries)... )
  @inbounds entstr[end] = entstr[end][1:end-1]
  vcat( "{", string.( "  ", entstr ), "}" )
end


function processpair( first::AbstractString, second )
  secstr = processexpr(second)
  secstr isa Vector || return string( first, ": ", secstr, ',' )
  @inbounds secstr[1] = string( first, ": {" )
  @inbounds secstr[end] = string( secstr[end], ',' )
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
  elexpr isa Nothing || (elstr = processexpr(elexpr) )
  !(isblockexpr(ifexpr) || isblockexpr(elexpr)) &&
    return string( condstr, " ? ", exprstr, " : ", elstr )
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


processreturn( expr ) = string( "return ", processexpr(expr) )


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


processthrow( expr ) = string( "throw ", processexpr(expr) )


function processanonfunction( fvars, fbody )
  vstr = processexpr(fvars)
  bstr = processexpr(fbody)
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

end
