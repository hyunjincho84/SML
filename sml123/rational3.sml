signature RATIONAL_A = 
sig
datatype rational = Frac of int * int 
					| Whole of int
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end

signature RATIONAL_B =
sig
type rational (* type now abstract *)
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end
	
signature RATIONAL_C =
sig
type rational (* type still abstract *)
exception BadFrac
val Whole : int -> rational 
   (* client knows only that Whole is a function *)
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end
(*gcd->약분을 위해 최소공배수 구해주는 애임*)
(*RATIONAL_A로 참조하면 make_frac없이 FRAC(1,2)이런식으로 만들 수 있음*)
structure Rational1 :> RATIONAL_C = (* can ascribe any of the 3 signatures above *)
struct

(* Invariant 1: all denominators > 0
   Invariant 2: rationals kept in reduced form *)
(*Frac (분자, 분모)*)
  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

(* gcd and reduce help keep fractions reduced, 
   but clients need not know about them *)
(* they _assume_ their inputs are not negative *)

(*정수 가능하면 정수도 나타내고, 가분수면 그냥 가분수로 나타냄몇으로 약분 가능한지 나타냄*)
	fun gcd (x,y) =
       if x=y
       then x
       else if x < y
       then gcd(x,y-x)
       else gcd(y,x)
(*whole이 의미하는거는 정수임*)
   fun reduce r =
       case r of
	   Whole _ => r
	 | Frac(x,y) => 
	   if x=0
	   then Whole 0
	   else let val d = gcd(abs x,y) in (* using invariant 1 *)
		    if d=y 
		    then Whole(x div d) 
		    else Frac(x div d, y div d) 
		end

(* when making a frac, we forbid zero denominators *)
   fun make_frac (x,y) =
       if y = 0
       then raise BadFrac
       else if y < 0
       then reduce(Frac(~x,~y))
       else reduce(Frac(x,y))

(* using math properties, both invariants hold of the result
   assuming they hold of the arguments *)
   fun add (r1,r2) = 
       case (r1,r2) of
	   (Whole(i),Whole(j))   => Whole(i+j)
	 | (Whole(i),Frac(j,k))  => Frac(j+k*i,k)
	 | (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
	 | (Frac(a,b),Frac(c,d)) => reduce (Frac(a*d + b*c, b*d))

(* given invariant, prints in reduced form *)
   fun toString r =
       case r of
	   Whole i => Int.toString i
	 | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)

end

(* this structure can have all three signatures we gave
   Rationa1, and/but it is /equivalent/ under signatures 
   RATIONAL_B and RATIONAL_C 

   this structure does not reduce fractions until printing
*)

(* Invariant 1: all denominators > 0
   Invariant 2: toString gives a reduced representation of
                the rational number*)

structure Rational2 :> RATIONAL_C (* or B or C *) =
struct
  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

  (* make_frac and add *)
   fun make_frac (x,y) =
       if y = 0
       then raise BadFrac
       else if y < 0
       then Frac(~x,~y)
       else Frac(x,y)

(* using math properties, both invariants hold of the result
   assuming they hold of the arguments *)
   fun add (r1,r2) = 
       case (r1,r2) of
	   (Whole(i),Whole(j))   => Whole(i+j)
	 | (Whole(i),Frac(j,k))  => Frac(j+k*i,k)
	 | (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
	 | (Frac(a,b),Frac(c,d)) => (Frac(a*d + b*c, b*d))


  (* toString (frac is reduced here) *)
   fun toString r =
   let 
      fun gcd (x,y) =
           if x=y
           then x
           else if x < y
           then gcd(x,y-x)
           else gcd(y,x)
       fun reduce r =
           case r of
	       Whole _ => r
	     | Frac(x,y) => 
	       if x=0
	       then Whole 0
	       else let val d = gcd(abs x,y) in (* using invariant 1 *)
		        if d=y 
		        then Whole(x div d) 
		        else Frac(x div d, y div d) 
		    end
   in
       case reduce r of
            Whole x => Int.toString x
          | Frac(x, y) => Int.toString x ^ "/" ^ Int.toString y
   end
end

(* this structure uses a different abstract type.  
   It does not even have signature RATIONAL_A.  
   For RATIONAL_C, we need a function Whole.  
*) 
(*얘가 rational2랑 다른거는 rational을 그냥 int*int로 줌  *)
structure Rational3 :> RATIONAL_B (* or C *)= 
struct 
   type rational = int * int
   exception BadFrac
	     
   (* int*int->int*int
      int*int->rational *)
   fun make_frac (x,y) = 
       if y=0
       then raise BadFrac
       else if y<0
       then (~x, ~y)
       else (x, y)

   fun Whole(i) = (i, 1)
     (* 'a -> 'a*int 
     *
     * int -> rational
     *)

   fun add ((a,b),(c,d)) = (a*d + c*b, b*d)
     (* ((int*int) * (int*int)) -> (int*int) *)
     (*  (rational * rational ) -> rational *)
(*메인에서 Rational.toString(1,2)이런 식으로는 안됨 signiture에 rational ->string이라서*)
   fun toString (x,y) =
       if x=0
       then "0"
       else
	   let fun gcd (x,y) =
		   if x=y
		   then x
		   else if x < y
		   then gcd(x,y-x)
		   else gcd(y,x)
(*gcd함수랑 밑에있는 val들 먼저 계산하고 in으로 들어감*)
	     val d = gcd (abs x,y)    (*abs ~2하면 그냥 2 나옴*)(*d는 약분을 위한 최소공배수*)
	     val num = x div d        (*분자 약분*)
	     val denom = y div d      (*분모 약분*)
	   in
	     if denom=1
	     then Int.toString num
	     else Int.toString num ^ "/" ^ Int.toString denom
	   end
end
