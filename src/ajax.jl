processajaxcall( ajaxcall::Symbol, cargs... ) =
  haskey( AJAX_CALLS, ajaxcall) ? AJAX_CALLS[ajaxcall](cargs...) : processfcall( Symbol("@AJAX"), ajaxcall, cargs... )

ajaxrequest() = :(XMLHttpRequest()) |> processexpr


function openrequest( req, protocol::StrSymb, address, async::Bool=true )
  pstr = string(protocol)
  :($req.open( $pstr, $address, $async )) |> processexpr
end


sendrequest( req ) = :($req.send()) |> processexpr
sendrequest( req, payload ) = :($req.send($payload)) |> processexpr


setheader( req, header, value ) = :($req.setRequestHeader( $header, $value )) |> processexpr


getheader( req ) = :($req.getAllResponseHeaders()) |> processexpr
getheader( req, header ) = :($req.getResponseHeader($header)) |> processexpr


abortrequest( req ) = :($req.abort()) |> processexpr


AJAX_CALLS = Dict(
  :Request => ajaxrequest,
  :openRequest => openrequest,
  :sendRequest => sendrequest,
  :setRequestHeader => setheader,
  :getResponseHeader => getheader,
  :abortRequest => abortrequest,
)
