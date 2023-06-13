if !isdefined( Main, :Julia2JavaScript )
  include("src/Julia2JavaScript.jl")
  using Main.Julia2JavaScript
end


begin
  println( 5, " becomes ", @jscript 5 )
  println( "\"Test\" becomes ", @jscript "Test" )

  println( "output( :console, a + 5 ) becomes ", @jscript output( :console, a + 5 ) )
  println( "output( :alert, a + 5 ) becomes ", @jscript output( :alert, a + 5 ) )
  println( "output( :doc, a + 5 ) becomes ", @jscript output( :doc, a + 5 ) )
  
  println( Dict( :a => 3, :b => 5 ), " becomes ", @jscript Dict( :a => 3, :b => 5 ) )
  println( Dict( :a => 3, :b => Dict( :c => 2, :d => 3 ) ), " becomes ", @jscript Dict( :a => 3, :b => Dict( :c => 2, :d => 3 ) ) )
  println( Dict( "a" => 3, "b" => 5 ), " becomes ", @jscript Dict( "a" => 3, "b" => 5 ) )
  println( Dict( "a" => 3, :b => 5 ), " becomes ", @jscript Dict( "a" => 3, :b => 5 ) )

  println( "x = 5 becomes ", @jscript x = 5 )
  println( "x = 5, 6 becomes ", @jscript x = 5, 6 )
  println( "x, y = 5, 6 becomes ", @jscript x, y = 5, 6 )
  println( "x, y = 5 becomes ", @jscript x, y = 5 )
  println( "x, y, z = 5, 6 becomes ", @jscript x, y, z = 5, 6 )
  println( "x, y = 5, 6, 7 becomes ", @jscript x, y = 5, 6, 7 )
  println( """y = Dict( :a => 3, :b => "test" ) becomes """, @jscript y = Dict( :a => 3, :b => "test" ) )
  println( "a = b = 3 becomes ", @jscript a = b = 3 )

  println( "a + 4 becomes ", @jscript a + 4 )
  println( "a + 4 + b becomes ", @jscript a + 4 + b )
  println( "a - 4 + b becomes ", @jscript a - 4 + b )
  println( "a - 4 * b becomes ", @jscript a - 4 * b )
  println( """string( "John", " ", "Doe" ) becomes """, @jscript string( "John", " ", "Doe" ) )
  
  println( "@var x = 5 becomes ", @jscript @var x = 5 )
  println( "@let y becomes ", @jscript @let y )
  println( "@const x = 5 becomes ", @jscript @const x = 5 )

  println( "a + 4 becomes ", @jscript a + 4 )
  println( "a - 4 becomes ", @jscript a - 4 )
  println( "a * 4 becomes ", @jscript a * 4 )
  println( "a / 4 becomes ", @jscript a / 4 )
  println( "a ^ 4 becomes ", @jscript a ^ 4 )
  println( "a % 4 becomes ", @jscript a % 4 )
  println( "inc(a) becomes ", @jscript inc(a) )
  println( "inc(a, true) becomes ", @jscript inc(a, true) )
  println( "dec(a) becomes ", @jscript dec(a) )
  println( "dec(a, true) becomes ", @jscript dec(a, true) )

  println( "@let x = 100 + 50 * 3 becomes ", @jscript @let x = 100 + 50 * 3 )

  println( "x = 5 becomes ", @jscript x = 5 )
  println( "x += 5 becomes ", @jscript x += 5 )
  println( "x -= 5 becomes ", @jscript x -= 5 )
  println( "x /= 5 becomes ", @jscript x *= 5 )
  println( "x *= 5 becomes ", @jscript x /= 5 )
  println( "x ^= 5 becomes ", @jscript x ^= 5 )
  println( "x %= 5 becomes ", @jscript x %= 5 )

  println( "a == 5 becomes ", @jscript a == 5 )
  println( "a === 5 becomes ", @jscript a === 5 )
  println( "a != 5 becomes ", @jscript a != 5 )
  println( "a !== 5 becomes ", @jscript a !== 5 )
  println( "a > 5 becomes ", @jscript a > 5 )
  println( "a < 5 becomes ", @jscript a < 5 )
  println( "a >= 5 becomes ", @jscript a >= 5 )
  println( "a <= 5 becomes ", @jscript a <= 5 )
  println( "true ? a : 5 becomes ", @jscript true ? a : 5 )

  println( """string( text1, " ", text2 ) becomes """, @jscript string( text1, " ", text2 ) )

  println( "a && b becomes ", @jscript a && b )
  println( "a || b becomes ", @jscript a || b )
  println( "!b becomes ", @jscript !b )

  println( "a & b becomes ", @jscript a & b )
  println( "a | b becomes ", @jscript a | b )
  println( "~b becomes ", @jscript ~b )
  println( "a ⊻ b becomes ", @jscript a ⊻ b )
  println( "a << b becomes ", @jscript a << b )
  println( "a >> b becomes ", @jscript a >> b )
  println( "a >>> b becomes ", @jscript a >>> b )

  println( "typeof(a) becomes ", @jscript typeof(a) )

  println( """function myFunction( p1::Int, p2 )
    a = 2 * p1 + 1
    return a * p2
  end becomes """, @jscript function myFunction( p1::Int, p2 )
    a = 2 * p1 + 1
    return a * p2
  end )
  println( "myFunction( a, b ) becomes ", @jscript myfunction( a, b ) )
  println( "myFunction( Dict(:d => 3), b ) becomes ", @jscript myfunction( Dict(:d => 3), b ) )

  println( "a.b becomes ", @jscript a.b )
  println( "a[1] becomes ", @jscript a[1] )
  println( "a[b] becomes ", @jscript a[b] )
  println( """a["b"] becomes """, @jscript a["b"] )
  println( "a.b(4, c) becomes ", @jscript a.b(4, c) )
  
  println( """@template "Hello World!" becomes """, @jscript @template "Hello World!" )
  println( """@template "Welcome \$firstName, \$(lastName)!" becomes """, @jscript @template "Welcome $firstName, $(lastName)!" )
  println( """@template "Total: \$((price * (1 + VAT)).toFixed(2))" becomes """, @jscript @template "Total: $((price * (1 + VAT)).toFixed(2))" )

  println( "@let x = 0xff becomes ", @jscript @let x = 0xff )
  println( "@let x = @new Number(123) becomes ", @jscript @let x = @new Number(123) )

  println( "(123).toString() becomes ", @jscript (123).toString() )
  println( """Number(@new Date("1970-01-01")) becomes """, @jscript Number(@new Date("1970-01-01")) )

  println( """@const cars = ["Saab", "Volvo", "BMW"] becomes """, @jscript @const cars = ["Saab", "Volvo", "BMW"] )
  println( """@const cars = [] becomes """, @jscript @const cars = [] )
  println( """cars[1] = "Saab" becomes """, @jscript cars[1] = "Saab" )
  println( "length(cars) becomes ", @jscript length(cars) )
  println( "sort(cars) becomes ", @jscript sort(cars) )
  println( "@let fruit = fruits[end] becomes ", @jscript @let fruit = fruits[end] )
  println( "foreach( myFunction, fruits ) becomes ", @jscript foreach( myFunction, fruits ) )
  println( """push!( fruits, "Lemon" ) becomes """, @jscript push!( fruits, "Lemon" ) )

  println( "π becomes ", @jscript π )
  println( "ℯ becomes ", @jscript ℯ )
  println( "round(4.6) becomes ", @jscript round(4.6) )
  println( "ceil(4.6) becomes ", @jscript ceil(4.6) )
  println( "floor(4.6) becomes ", @jscript floor(4.6) )
  println( "trunc(4.6) becomes ", @jscript trunc(4.6) )
  println( "sqrt(4.6) becomes ", @jscript sqrt(4.6) )
  println( "abs(4.6) becomes ", @jscript abs(4.6) )
  println( "sin(4.6) becomes ", @jscript sin(4.6) )
  println( "cos(4.6) becomes ", @jscript cos(4.6) )
  println( "min(0, 150, 30, 20, -8, -200) becomes ", @jscript min(0, 150, 30, 20, -8, -200))
  println( "max(0, 150, 30, 20, -8, -200) becomes ", @jscript max(0, 150, 30, 20, -8, -200) )
  println( "log(4.6) becomes ", @jscript log(4.6) )
  println( "log2(4.6) becomes ", @jscript log2(4.6) )
  println( "log10(4.6) becomes ", @jscript log10(4.6) )

  println( "rand() becomes ", @jscript rand() )
  println( "rand(0:3) becomes ", @jscript rand(0:3) )
  println( "rand(1:5) becomes ", @jscript rand(1:5) )
  println( "rand(2:1.5:12) becomes ", @jscript rand(2:1.5:12) )
  println( "rand(10:-2:-1) becomes ", @jscript rand(10:-2:-1) )

  println( """if hour < 18
    greeting = "Good day"
  end becomes """, @jscript if hour < 18
    greeting = "Good day"
  end )
  println( """if hour < 18
    greeting = "Good day"
  else
    greeting = "Good evening"
  end becomes """, @jscript if hour < 18
    greeting = "Good day"
  else
    greeting = "Good evening"
  end )
  println( """if hour < 10
    greeting = "Good morning"
  elseif hour < 20
    greeting = "Good day"
  else
    greeting = "Good evening"
  end becomes """, @jscript if hour < 10
    greeting = "Good morning"
  elseif hour < 20
    greeting = "Good day"
  else
    greeting = "Good evening"
  end )

  println( "break becomes ", @jscript break )
  println( "continue becomes ", @jscript continue )

  println( """for ii in 1:10
    text += string( cars[ii], "<br>" )
  end becomes """, @jscript(for ii in 1:10
    text += string( cars[ii], "<br>" )
  end) )
  println( """for ii in eachindex(cars)
    text += string( cars[ii], "<br>" )
  end becomes """, @jscript(for ii in eachindex(cars)
    text += string( cars[ii], "<br>" )
  end) )
  println( """for ii in 1:10, jj in 3:2:11
    ii * jj
  end becomes """, @jscript(for ii in 1:10, jj in 3:2:11
    ii * jj
  end) )
  println( """for x in keys(person)
    text += person[x]
  end becomes """, @jscript(for x in keys(person)
    text += person[x]
  end) )
  println( """for (x, pers) in person
    text += pers
  end becomes """, @jscript(for (x, pers) in person
    text += pers
  end) )
  println( """for x in cars
    text += x
  end becomes """, @jscript(for x in cars
    text += x
  end) )

  println( """while i < 10
    text += string( "The number is ", i )
    i += 1
  end becomes """, @jscript(while i < 10
    text += string( "The number is ", i )
    i += 1
  end) )
  
  println( r"w3schools"i, " becomes ", @jscript r"w3schools"i )
  println( r"w3schools", " becomes ", @jscript r"w3schools" )
  
  println( """try
    adddlert("Welcome guest!");
  catch err
    document.getElementById("demo").innerHTML = err.message;
  end becomes """, @jscript try
    adddlert("Welcome guest!");
  catch err
    document.getElementById("demo").innerHTML = err.message;
  end )
  println( """throw("Too big") becomes """, @jscript throw("Too big") )
  println( """try
    adddlert("Welcome guest!")
  catch err
    document.getElementById("demo").innerHTML = err.message
  finally
    ecount += 1
  end becomes """, @jscript try
    adddlert("Welcome guest!")
  catch err
    document.getElementById("demo").innerHTML = err.message
  finally
    ecount += 1
  end )

  println( """@usestrict becomes """, @jscript @usestrict )

  println( "(a, b) -> a * b becomes ", @jscript (a, b) -> a * b )
  println( """() -> "Hello World!" becomes """, @jscript () -> "Hello World!" )
  println( """val -> string( "Hello ", val ) becomes """, @jscript val -> string( "Hello ", val ) )
end

begin
  println( "element = @DOM getElement intro id becomes ", @jscript element = @DOM getElement intro id )
  println( "x = @DOM getElement main id becomes ", @jscript x = @DOM getElement main id )
  println( "y = @DOM getElement x p tag true becomes ", @jscript y = @DOM getElement x p tag true )
  println( "x = @DOM getElement intro class true becomes ", @jscript x = @DOM getElement intro class true )
  println( "x = @DOM getElement x p.intro true becomes ", @jscript x = @DOM getElement x p.intro true )

  println( """@DOM changeHTML (@DOM getElement p1 id) "New text!" true becomes """, @jscript @DOM changeHTML (@DOM getElement p1 id) "New text!" true )
  println( """@DOM changeHTML element "New Heading" true becomes """, @jscript @DOM changeHTML element "New Heading" true )
  println( """@DOM changeAttribute (@DOM getElement myImage id) src "landscape.jpg" becomes """, @jscript @DOM changeAttribute (@DOM getElement myImage id) src "landscape.jpg" )
  println( """@jscript @DOM changeHTML (@DOM getElement demo id) string( "Date : ", Date() ) becomes """, @jscript @DOM changeHTML (@DOM getElement demo id) string( "Date : ", Date() ) )

  println( "@DOM getStyle x color becomes ", @jscript @DOM getStyle x color )
  println( """@DOM setStyle (@DOM getElement p2 id) color "blue" becomes """, @jscript @DOM setStyle (@DOM getElement p2 id) color "blue" )
  println( """@DOM setStyle (@DOM getElement id1 id) color "red" becomes """, @jscript @DOM setStyle (@DOM getElement id1 id) color "red" )
  
  println( "@DOM addEvent (@DOM getElement myBtn id) click displayDate becomes ", @jscript @DOM addEvent (@DOM getElement myBtn id) click displayDate )
  println( """@DOM addEvent window resize function()
    @DOM changeHTML (@DOM getElement demo id) sometext true
  end becomes """, @jscript @DOM addEvent window resize function()
    @DOM changeHTML (@DOM getElement demo id) sometext true
  end )
  println( """@DOM addEvent element click function()
    myFunction( p1, p2 )
  end becomes """, @jscript @DOM addEvent element click function()
    myFunction( p1, p2 )
  end )
  println( "@DOM removeEvent element mousemove myFunction becomes ", @jscript @DOM removeEvent element mousemove myFunction )

  println( """const para = @DOM createElement p becomes """, @jscript const para = @DOM createElement p )
  println( """const node = @DOM createText "This is new." becomes """, @jscript const node = @DOM createText "This is new." )
  println( "@DOM appendChild element para becomes ", @jscript @DOM appendChild element para )
  println( "@DOM insertBefore element para child becomes ", @jscript @DOM insertBefore element para child )
  println( "@DOM removeChild parent child becomes ", @jscript @DOM removeChild parent child )
  println( "@DOM replaceChild parent para child becomes ", @jscript @DOM replaceChild parent para child )
end

begin
  println( "@timeout myFunction 3000 becomes ", @jscript @timeout myFunction 3000 )
  println( """@timeout function ()
    myFunction("I love You !!!")
  end 3000 becomes """, @jscript @timeout function ()
    myFunction("I love You !!!")
  end 3000 )
  println( "@timeout myFunction 1000 true becomes ", @jscript @timeout myFunction 1000 true )

  println( """@let myPromise = @promise myResolve myReject begin
    @let x = 0
    
    if x == 0
      myResolve("OK")
    else
      myReject("Error")
    end
  end becomes """, @jscript @let myPromise = @promise myResolve myReject begin
    @let x = 0
    
    if x == 0
      myResolve("OK")
    else
      myReject("Error")
    end
  end )
  println( "@let myPromise = @promise promiseFunc becomes ", @jscript @let myPromise = @promise promiseFunc )
  
  println( "myPromise |> myResolve becomes ", @jscript myPromise |> myResolve )
  println( """myPromise |> function(value)
  myDisplayer(value)
  end becomes """, @jscript myPromise |> function(value)
  myDisplayer(value)
  end )
  println( """myPromise |> [function(value)
  myDisplayer(value)
  end] becomes """, @jscript myPromise |> [function(value)
  myDisplayer(value)
  end] )
  println( """myPromise |> (
    function(value)
      myDisplayer(value)
    end,
    function(error)
      myDisplayer(error)
    end
  ) becomes """, @jscript myPromise |> (
    function(value)
      myDisplayer(value)
    end,
    function(error)
      myDisplayer(error)
    end
  ) )
  
  println( """@async function myFunction()
    return "Hello";
  end becomes """, @jscript @async function myFunction()
    return "Hello";
  end )
  println( "@let value = (@await promise) becomes ", @jscript @let value = (@await promise) )
end

begin
  println( "const xhttp = @new @AJAX Request becomes ", @jscript const xhttp = @new @AJAX Request )

  println( """@AJAX openRequest xhttp GET "ajax.info.txt" becomes """, @jscript @AJAX openRequest xhttp GET "ajax.info.txt" )
  println( "@AJAX sendRequest xhttp becomes ", @jscript @AJAX sendRequest xhttp )
  println( """@AJAX setResponseHeader xhttp "Content-type" "application/x-www-form-urlencoded" becomes """, @jscript @AJAX setResponseHeader xhttp "Content-type" "application/x-www-form-urlencoded" )
  println( """@AJAX sendRequest xhttp "fname=Henry&lname=Ford" becomes """, @jscript @AJAX sendRequest xhttp "fname=Henry&lname=Ford" )

  println( "@AJAX getResponseHeader this becomes ", @jscript @AJAX getResponseHeader this )
  println( """@AJAX getResponseHeader this "Last-Modified" becomes """, @jscript @AJAX getResponseHeader this "Last-Modified" )

  println( "@AJAX abortRequest xhttp becomes ", @jscript @AJAX abortRequest xhttp )
end

