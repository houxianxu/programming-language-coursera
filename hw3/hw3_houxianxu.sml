 (*Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(*1*)
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

(*2*)
val longest_string1 =
	List.foldl (fn (x, acc) => if String.size x > String.size acc
							   then x 
							   else acc)
				"" 

(*3*)
val longest_string2 =
	List.foldl (fn (x, acc) => if String.size x >= String.size acc
							   then x 
							   else acc)
				"" 

(*4*)
fun longest_string_helper f xs =
	List.foldl (fn (x, acc) => if f(String.size x, String.size acc) 
							   then x
							   else acc)
				""
				xs 
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*5*)
val longest_capitalized = longest_string1 o only_capitals 

(*6*)
val rev_string =String.implode o List.rev o String.explode 

(*7*)
fun first_answer f xs = 
	case xs of
		[] => raise NoAnswer
		| x::xs' => case f x of
			NONE => first_answer f xs'
			| SOME v => v

(*8*)
fun all_answers f xs =
	let
		fun aux (acc, xs) =
			case xs of
				[] => SOME acc
				| x::xs' => case f x of
							NONE => NONE
							| SOME x => aux (x @ acc, xs')
						
	in
		aux ([], xs)
	end

(*9a*)
fun count_wildcards p = 
	g (fn () => 1) (fn x => 0) p
(*9b*)
fun count_wild_and_variable_lengths p =  
	g (fn () => 1) (fn s => String.size s) p
(*9c*)
fun count_some_var (s, p) =
	g (fn() => 0) (fn s' => if s = s' then 1 else 0) p

(*10*)
fun check_pat p = 
    let
        fun all_variables p = case p of
                                Variable x  => [x]
                              | TupleP xs   => List.foldl (fn (p, acc) => all_variables(p) @ acc) [] xs
                              | ConstructorP(_, p) => all_variables p
                              | _ => []
        
        
        fun repeat xs =
          case xs of
            [] => false
          | x::xs' => List.exists (fn y => y = x) xs' orelse repeat xs' 
    in
        not ( repeat( all_variables p ) )
    end

(*11*)
fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
		| (v, Variable x) => SOME [(x, v)]
		| (Unit, UnitP) => SOME []
		| (Const x, ConstP y) => if x = y then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if List.length vs = List.length ps
								   then all_answers (fn (v, p) => match(v, p)) (ListPair.zip(vs, ps))
								   else NONE
        |(Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 
	        									     then match (v, p)
	        									     else NONE
        | _ => NONE

(*12*)
fun first_match v ps =
	SOME (first_answer (fn p => match(v, p)) ps)
	handle NoAnswer => NONE