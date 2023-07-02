(*fun f1 x = 4 div "hi" (* but f1 never called *) 
fun f2 x = if true then 0 else 4 div "hi"
fun f3 x = if x then 0 else 4 div "hi" val x = f3 true
fun f4 x = if x <= abs x then 0 else 4 div "hi"
fun f5 x = 4 div x val y = f5 (if true then 1 else "hi")
*)
val cbs : (int -> unit) list ref = ref []
fun onKeyEvent f =  cbs := f :: (!cbs) fun onEvent i =
let fun loop fs =
case fs of
[]     => () | f::fs' => (f i; loop fs')
in loop (!cbs) end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ =>
timesPressed := (!timesPressed) + 1)
fun printIfPressed i = onKeyEvent (fn j =>
if i=j then print ("pressed " ^ Int.toString i) else ())

