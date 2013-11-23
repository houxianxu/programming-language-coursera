val x = 1 (* x maps to 1*)
fun f y = x + y (* f maps to a function that add 1 to its argument*)
val x = 2 (* x maps to 2*)
val y = 3 (* y maps to 3*)
val z = f (x + y) (* z maps to 6*)


(* Closure => a function has two parts: the code and environment*)

fun f g =
	let
		val x = 3 (* irrelevant *)
	in
		g 2
	end

val x = 4

fun h y = x + y (* add 4 to its argument *)
val z = f h (* 6 *)

(* the rule is that a function body is evaluaed i the environment 
where the function was defined (created)*)
	(* -- Extended with the function argument *)

fun filter (f, xs) =
	case xs of
		[] => []
		| x::xs' => if f x then x::(filter(f, xs'))
					else filter(f, xs')
fun allShorterThan1 (xs, s) =
	filter(fn x => String.size x < String.size s, xs)
fun allShorterThan2 (xs, s) = 
	let
		val i = String.size s
	in
		filter(fn x => String.size x < String.size s, xs)
	end

(*Closure*)
fun fold (f, acc, xs) =
	case xs of
		[] => acc
		| x::xs' => fold(f, f(acc, x), xs') 

fun f1 xs = fold ((fn (x, y) => x + y), 0, xs) (*sum list*)

fun f2 xs = fold ((fn (x, y) => x andalso y >= 0), true, xs)

fun f3 (xs, lo, hi) =
	fold ((fn (x, y) => 
				x + (if y >= lo andalso y <= hi then 1 else 0)),
		   0, xs)
(*combing function*)
fun compose(f, g) = fn x => f(g, x)
fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

(* |> 3 + 4*)
infix |>
fun x |> f = f x

fun sqrt_of_abs i = i |> abs |> Real.fromInt |>Math.sqrt

fun backup1 (f, g) = fn x => case f x of
								NONE => g x
								| SOME y  => y	
fun backup2 (f, g) = fn x => f x handle _ => g x



(*Curring 
	Take one arugement and return a function that takes another arument and ...*)

fun sorted3_tupled (x, y, z) = z >= y andalso y >= x
val t1 = sorted3_tupled (7, 9, 11)

val sorted3 = fn x => fn y => fn z => z >= y andalso y>= x
(*fun sorted3 x = fn y => fn z => ...*)
val t2 = ((sorted3 7) 9) 11
(* syntax sugar*)
val t3 = sorted3 7 9 11
(*
	val wrong1 = sorted3_tupled 7 9 11
	val wrong2 = sorted3 (7, 9, 11) 
*)
(*syntax sugar for line 78*)
fun sorted3_nicer x y z = z >= y andalso y >= x



(*partial application*)


fun range i j = 
	if i > j then [] 
	else i :: range (i + 1) j
val countup = range 1
(*countup 6 [1, 2, 3, 4, 5, 6]*)

fun exists predicate xs =
	case xs of
		[] => false
		| x::xs' => predicate x orelse exists predicate xs'

val haszero = exists (fn x => x = 0) (* int list -> bool*)

val incrementAll = List.map(fn x => x + 1) (*int list -> int list*)


(*
	currying wrapup
*)
fun curry f = fn x => fn y => f (x, y)
fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y 
fun other_curry f x y = f (y, x)

fun range(i, j) = if i > j then []
				  else i :: range(i+1, j)

val countup = curry range 1
val xs = countup 7 (* [1, 2, 3, 4, 5, 6, 7*)


(*
	Callbacks
*)
