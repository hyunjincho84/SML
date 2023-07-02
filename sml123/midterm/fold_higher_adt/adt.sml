(* abstract data type using closures *)

(* int set 
 * with 3 methods: insert, member, size, 
 *             notice insert returns a set (a new set)
 *)

datatype set = S of { insert : int -> set, 
                  member : int -> bool,  
                  size   : unit -> int
                }

(* implementation of sets: this is the fancy stuff, but clients using
   this abstraction do not need to understand it *)
val empty_set =
  let
    fun make_set xs = 
        let fun contains i = List.exists (fn x => i=x) xs
        in
            S({insert = fn i => if contains(i)
                                then make_set(xs)
                                else make_set(i::xs),
                member = contains,
                size   = fn () => length(xs)
            })
        end
  in
    make_set [] (*  set value S{...}*)
  end 

(* example client *)
val S(s1) = empty_set;

val S(s2) = (#insert s1) 34;  (* s2 has xs=[34] *)
val S(s3) = (#insert s2) 34;  (* s3 xs=[34] *)
val S(s4) = (#insert s3) 19;  (* s4 xs = [34, 19] *)
(#member s4) 19


