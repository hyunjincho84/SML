(*--------------sol1----------------*)
datatype expr = NUM of int
                  | PLUS of expr * expr
                  | MINUS of expr * expr

datatype formula = TRUE
                     | FALSE
                     | NOT of formula
                     | ANDALSO of formula * formula
                     | ORELSE of formula * formula
                     | IMPLY of formula * formula
                     | LESS of expr * expr
                        (* LESS(a, b) is true if a < b *)

fun evaluate e =
	case e of 
			NUM i			=> i
		|	PLUS(e1,e2)		=> (evaluate e1) + (evaluate e2)
		|	MINUS(e1,e2)	=> (evaluate e1) - (evaluate e2)

fun eval e = 
	case e of
			TRUE			=> true
		|	FALSE			=> false
		|	NOT		e1		=> not(eval(e1))
		|	ANDALSO	(e1,e2)	=> eval(e1) andalso eval(e2)
		|	ORELSE	(e1,e2)	=> eval(e1) orelse eval(e2)
		|	IMPLY	(e1,e2)	=> not(eval(e1)) orelse eval(e2)
		|	LESS	(e1,e2)	=> evaluate(e1) < evaluate(e2)


(*--------------sol2----------------*)

type name = string
    datatype metro = STATION of name
                   | AREA of name * metro
	               | CONNECT of metro * metro



fun checkMetro m = 
	let fun isInList (x, l:string list) = 
		if x = hd l
		then true
		else 
			if tl l = []
			then false
			else isInList(x, tl l)
	in
		let fun checkalpha (m, list1) =
					case m of
					STATION n1			=>	isInList(n1,list1)
				|	AREA	(n1, m1)	=>	checkalpha(m1, n1::list1)
				|	CONNECT	(m1, m2)	=>	checkalpha(m1,list1) andalso checkalpha(m2,list1)
		in
			checkalpha(m,[])
		end
	end



(*--------------sol3----------------*)

datatype 'a lazyList = nullList
					| cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) =
    let
        fun seqsub(first1) =
            if first1 > last 
			then nullList
            else cons(first1, fn () => seqsub(first1 + 1))
    in
        seqsub(first)
    end

fun infSeq(first) =
	let
		fun infSeqsub(first1) = 
			cons(first1, fn () => infSeqsub(first1 + 1))
	in
		infSeqsub(first)
	end

fun firstN(lazyListVal, n) =  
	if n = 0
	then []
	else 
		case lazyListVal of
			nullList 				=>	[]
			|	cons (valu, func)	=>	valu::firstN(func(), n - 1)

fun Nth(lazyListVal, n) =
	let fun nthsub(nullList, _) = NONE
		|	nthsub(cons(valu, func), 1) = SOME(valu)
		|	nthsub(cons(valu, func), n) = nthsub(func(), n - 1)
	in
		nthsub(lazyListVal, n)
	end

fun filterMultiples(lazyListVal, n) = 
	case lazyListVal of
		nullList	=> nullList
		|	cons(valu, func)	=> if valu mod n = 0
									then filterMultiples(func(),n)
									else cons(valu,fn()=>filterMultiples(func(), n))

(*------------------------------------------------------------------------------*)

fun primes() =
	let fun sieve(lazyListVal) = 
		case lazyListVal of
			nullList				=> nullList
			|	cons(valu, func)	=> cons(valu, fn()=>sieve(filterMultiples(func(), valu)))
	in 
		sieve(infSeq(2))
	end 

