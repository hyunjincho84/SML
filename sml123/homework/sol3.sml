datatype pattern = Wildcard | Variable of string | UnitP
                    | ConstP of int | TupleP of pattern list
                    | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
                  | Constructor of string * valu

(*---------------------------------sol1--------------------------------*)
fun check_pat p = 
	let fun variable(v) =
		case v of
			Variable v1 		=> [v1]
		|	TupleP p 			=> List.foldl (fn(v,vs)=>vs @ variable(v))[] p
		|	ConstructorP(_,p)	=> variable(p)
		|	_					=>[]
		fun a(s)=
			case s of
				[]=> true
			|	s::ss=>if List.exists (fn a => s=a) ss then false else a(ss)
	in
		a(variable(p))
	end


(*---------------------------------sol2--------------------------------*)
fun match(v, p) =
    case (v, p) of
        (_, Wildcard)               => SOME []
    |   (_, Variable s)           	=> SOME [(s, v)]
    |   (Unit, UnitP)               => SOME []
    |   (Const i, ConstP j)         => if i = j then SOME [] else NONE
    |   (Tuple vlist, TupleP plist) =>
        if   List.length(vlist) = List.length(plist)
        then let val lst = ListPair.zip(vlist,plist)
			in
				let fun input f xs init =
					case xs of 
						[]		=> init
					|	x::xs'	=> case f x of
									NONE 	=> NONE
								|	SOME v	=> input f xs' (SOME (case init of SOME pre => pre@v))
				in
					input match lst (SOME [])
				end
			end
		else NONE
    |   (Constructor(s1, v), ConstructorP(s2, p)) =>
        if   s1 = s2
        then match(v, p)
        else NONE
    |   (_, _)                      => NONE

(*---------------------------------sol3--------------------------------*)
type name = string
	datatype RSP =
			ROCK
        	| SCISSORS
        	| PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
     datatype tournament =
          PLAYER of name * (RSP strategy ref)
        | MATCH of tournament * tournament


fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
val sp = alterTwo(SCISSORS, PAPER)
val rps = alterThree(ROCK,PAPER, SCISSORS)
fun next(strategyRef) =
	let val Cons(rsp, func) = !strategyRef 
	in
    	strategyRef := func();
		rsp
	end

fun game(p1,p2) = 
	case (p1,p2) of
		(PLAYER (name1, rsp1), PLAYER(name2,rsp2))	=>	case (next(rsp1),next(rsp2)) of
		(ROCK, SCISSORS)	=>p1
	|	(ROCK, PAPER)		=>p2
	|	(PAPER, ROCK)		=>p1
	|	(PAPER, SCISSORS)	=>p2
	|	(SCISSORS, PAPER)	=>p1
	|	(SCISSORS, ROCK)	=>p2
	|	(_, _)				=>game(p1,p2)

fun whosWinner(t) =
	case t of
		MATCH(t1,MATCH(t2,t3))	=> whosWinner(MATCH(t2,t3))
	|	MATCH(MATCH(t1,t2), _)	=> whosWinner(MATCH(t1,t2))
	|	MATCH(t1,t2)			=> game(t1,t2)

val winner = whosWinner(MATCH(MATCH(PLAYER("s", ref s),PLAYER("sr", ref sr)), MATCH(PLAYER("ps", ref ps), PLAYER("r", ref r))));
	
