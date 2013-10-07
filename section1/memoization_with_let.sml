(*comute the largest item in a list*)

(*without memoizaiton *)
fun bad_max (xs : int list) =
	if null xs
	then 0
	else if null (tl xs)
	then hd xs
	else if (hd xs) > bad_max(tl xs)
	then hd xs
	else bad_max(tl xs)

(* with memoization using let*)
(* as far as I am concerned, this is much like use dictionary in python
   to remember the intermediate results. *)
(*becasue tl_ans is a local variable becasue of 'let', we could just use one
variable to memoize all the intermediate results, which is very clever*) 
fun good_max (xs : int list) =
	if (null xs)
	then 0
	else if (null (tl xs))
	then hd xs
	else 
		let
		    val tl_ans = good_max (tl xs)
		in
		    if (hd xs) > tl_ans
		    then hd xs
		    else tl_ans
		end

(*help function*)
fun countup (from : int, to : int) =
	if from=to then from :: [] else from :: countup(from+1, to) 