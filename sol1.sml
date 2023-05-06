(*----------sol1----------*)
fun merge(x:int list, y:int list) = 
		if null x orelse null y
		then 
			if null x
			then y
			else x
		else
			if hd x < hd y
			then hd x::merge(tl x, y)
			else hd y::merge(x, tl y)


(*----------sol2----------*)
fun input_num(x:int list) =
	let val result = []
	in
		x::result
	end

fun reverse(xs:int list) = 
	let fun rev(x:int list,y:int list) =
		if null (tl x)
		then hd x::y
		else
			let fun input_num(x:int list) =
					if null(tl x)
					then y
					else hd x::y
			in
				rev(tl x, input_num(x))
			end
	in
		rev(xs, [])
	end


(*----------sol3----------*)
fun pi (a:int, b:int, f:int->int):int =
	if a = b
	then f(b)
	else f(a) * pi(a + 1,b, f)


(*----------sol4----------*)
fun digits (x:int):int list =
	let fun digit_help (a:int) = 
		if a = 0
		then []
		else a mod 10::digit_help(a div 10)
	in reverse(digit_help(x))
	end


(*----------sol5----------*)
fun add_digit_once(x:int) = 
	let fun plus_digit(xs:int list) =
			if null xs
			then 0
			else (hd xs) + plus_digit(tl xs)
	in
		plus_digit(digits(x))
	end 

fun additivePersistence(x:int) =
		let val z = add_digit_once(x)
		in
			if z < 10
			then 1
			else additivePersistence(z)+1
		end

fun digitalRoot(x:int) = 
	if x < 10
	then x
	else digitalRoot(add_digit_once(x))

