processdomcall( domcall::Symbol, cargs... ) =
  haskey( DOM_CALLS, domcall ) ? DOM_CALLS[domcall](cargs...) : processfcall( Symbol("@DOM"), domcall, cargs... )


function getElement( source, elid, idtype::StrSymb, getall::Bool=false )
  fcall = string( "querySelector", getall ? "All" : "" ) |> Symbol
  idtype === :id && (elid = "#$elid")
  idtype === :class && (elid = ".$elid")
  idtype === :var || (elid = "$elid" )
  :( $source.$fcall($elid) ) |> processexpr
end

getElement( source, elid::QuoteNode, idtype::StrSymb, getall::Bool=false ) =
  getElement( source, elid.value, idtype, getall )
getElement( elid, idtype::StrSymb, getall::Bool=false ) = getElement( :document, elid, idtype, getall )
getElement( source, elid, getall::Bool=false ) = getElement( source, elid, :css, getall )
getElement( elid, getall::Bool=false ) = getElement( :document, elid, :css, getall )


function changeHTML( source, htmlstr, replace::Bool=false )
  expr = replace ? :($source.innerHTML = $htmlstr) : :($source.innerHTML += $htmlstr)
  processexpr(expr)
end


function getAttribute( source, attr::StrSymb, var::Bool=false )
  var || (attr = "$attr")
  :( $source.getAttribute($(attr)) ) |> processexpr
end


function changeAttribute( source, attr, value, var::Bool=false )
  var || (attr = "$attr")
  :( $source.setAttribute($attr, $value) ) |> processexpr
end

changeAttribute( source, attr, var::Bool=false ) =
  removeAttribute( source, attr, var )


function removeAttribute( source, attr, var::Bool=false )
  var || (attr = "$attr")
  :( $source.removeAttribute($attr) ) |> processexpr
end


function getStyle( source, style, var::Bool=false )
  var || (style = "$style")
  :( $source.style[$style] ) |> processexpr
end


function setStyle( source, style, val, var::Bool=false )
  var || (style = "$style")
  :( $source.style[$style] = $val ) |> processexpr
end


addEvent( source, event, func, useCapture::Bool=false, var::Bool=false ) =
  modifyEvent( source, event, func, useCapture, true )

removeEvent( source, event, func, useCapture::Bool=false, var::Bool=false ) =
  modifyEvent( source, event, func, useCapture, false )

function modifyEvent( source, event, func, useCapture, add::Bool, var::Bool=false )
  evsym = add ? :addEventListener : :removeEventListener
  var || (event = "$event")
  :( $source.$evsym( $event, $func, $useCapture ) ) |> processexpr
end


function createElement( el, var::Bool=false )
  var || (el = "$el")
  :( document.createElement($el) ) |> processexpr
end


createText( str ) = :( document.createTextNode($str) ) |> processexpr

appendChild( source, child ) = :( $source.appendChild($child) ) |> processexpr

insertBefore( source, child, loc ) =
  :( $source.insertBefore($child, $loc) ) |> processexpr
insertBefore( source, child ) = appendChild( source, child )

removeChild( source, child ) = :( $source.removeChild($child) ) |> processexpr

replaceChild( source, newchild, oldchild ) =
  :( $source.replaceChild($newchild, $oldchild) ) |> processexpr


DOM_CALLS = Dict(
  :getElement => getElement,
  :changeHTML => changeHTML,
  :getAttribute => getAttribute,
  :changeAttribute => changeAttribute,
  :removeAttribute => removeAttribute,
  :getStyle => getStyle,
  :setStyle => setStyle,
  :addEvent => addEvent,
  :removeEvent => removeEvent,
  :createElement => createElement,
  :createText => createText,
  :appendChild => appendChild,
  :insertBefore => insertBefore,
  :removeChild => removeChild,
  :replaceChild => replaceChild,
)


# end
