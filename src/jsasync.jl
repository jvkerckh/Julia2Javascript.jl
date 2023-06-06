function processtimeout( func, period::Integer, repeat::Bool=false )
  tfunc = repeat ? :setInterval : :setTimeout
  :($tfunc($func, $period)) |> processexpr
end


processpromise( resvar, rejvar, pbody ) =
  :(@new Promise( function ($resvar, $rejvar)
    $(pbody.args...)
  end )) |> processexpr

processpromise( pfunc ) =
  :(@new Promise($pfunc)) |> processexpr


processthen( promise, thenfunc::Symbol ) =
  :($promise.then($thenfunc)) |> processexpr

function processthen( promise, thenfuncs::Expr )
  thenfuncs.head ∈ [:tuple, :vect] && return processthen( promise, thenfuncs.args... )
  :($promise.then($thenfuncs)) |> processexpr
end

processthen( promise, resolvefunc, errorfunc ) =
  :( $promise.then($resolvefunc, $errorfunc)) |> processexpr


function processasync( fdef::Expr )
  fdef.head === :function || return processfcall( Symbol("@async"), fdef )
  fstr = processexpr(fdef)
  @inbounds fstr[1] = string( "async ", fstr[1] )
  fstr
end

processasync( fdef ) = processfcall( Symbol("@async"), fdef )


function processawait( expr )
  expstr = processexpr(expr)
  expstr isa String && return string( "await ", expstr )
  @inbounds expstr[1] = string( "await ", expstr[1] )
  expstr
end
