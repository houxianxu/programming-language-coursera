 fun fact n =
 	if n = 0 then 1
 	else n*fact(n-1)

 val x = fact 3


(* Tail recursion which is more effcient *)
fun fact n =
	let
	    fun aux(n, acc) =
	    	if n = 0
	    	then acc
	    	else aux(n-1, acc*n)
	in
	    aux(n, 1)
	end

(*
There is a methodology that can often guide this transformation:
	- Create a helper function that takes an accumulator
	- Old base case becomes initial accumulator
	- New base case becomes final accumulator

*)

 fun sum xs =
 	case xs of 
 		[] => 0
 	  | x::xs' => x + sum xs'
(* tail recursive version *)
fun sum1 xs =
	let
	    fun aux (xs, acc) = 
	       case xs of
	           [] => acc
	         | x:xs' => aux(xs', x + acc)
	in
	    body
	end


fun res xs = 
	case xs of
	    [] => []
	  | x::xs' => (res xs') @ [x]
  (* tail recursive version*)
fun rev xs =
	let fun aux(xs, acc) =
			case xs of
			    [] => acc
			  | x::xs' => aux(xs', x::acc)
	in 
		aux(xs, [])
	end

