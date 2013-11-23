(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*a*)
fun all_except_option (str, strs) = 
	case strs of
	    [] => NONE
	  | a::bs => case same_string(str, a) of
			      true => SOME bs
			    | false => case all_except_option(str, bs) of
					        NONE => NONE
					      | SOME b => SOME (a::b)

(*b*)
fun get_substitutions1 (substitutions, str) =
	case substitutions of
	    [] => []
	  | alist::lists => let
						    val tail_ans = get_substitutions1(lists, str)
						in
					      	case all_except_option(str, alist) of
							   NONE => tail_ans
						   	 | SOME str_list => tail_ans @ str_list

						end
(*c*)
fun get_substitutions2 (substitutions, str) = 
	let
	    fun aux (substitutions, acc) =
	    	case substitutions of
	    	    [] => acc
	    	  | alist::lists => case all_except_option(str, alist) of
					    	      NONE => aux(lists, acc)
					    	    | SOME str_list => aux(lists, acc @ str_list)
	in
	    aux(substitutions, [])
	end

(*d*)
fun similar_names (substitutions, full_name) = 
	let
		val {first = f, middle = m, last = l } = full_name
		val first_names = get_substitutions2(substitutions, f)
		fun cat_names (first_names) = 
			case first_names of
				[] => []
				| head::tail => {first = head, middle = m, last = l}::cat_names(tail)
	in
		full_name :: cat_names(first_names)
	end
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*a*)
fun card_color (suit, rank) =
	case suit of
		Clubs => Black
		| Spades => Black
		| _ => Red

(*b*)
fun card_value (suit, rank) =
	case rank of
		Num n => n
		| Ace => 11
		| _ => 10

(*c*)
fun remove_card (cs, c, e) =
	case cs of
		[] => raise e
		| head::tail => if head=c then tail
						else head::remove_card(tail, c ,e)

(*d*)
fun all_same_color (cs) =
	case cs of
		[] => true
		| _::[] => true
		| head1::head2::[] => (card_color head1)=(card_color head2)
		| head1::head2::tail => (card_color head1)=(card_color head2)
								andalso all_same_color(head2::tail)

(*e*)
fun sum_cards (cs) =
	let
		fun aux (cs, acc) = 
			case cs of
				[] => acc
				| head::tail => aux(tail, acc+card_value head)
	in
		aux(cs, 0)
	end

(*f*)
fun score (cs, goal) =
	let
		val sum = sum_cards(cs)
		val preliminary = 
			if sum > goal then 3 * (sum - goal)
			else goal - sum
	in
		if all_same_color cs then preliminary div 2
		else preliminary
	end

(*g*)
fun officiate (card_list, move_list, goal) =
	let
		fun aux (card_list, move_list, held)= 
			if sum_cards(held) > goal
			then score(held, goal)
			else case move_list of
				[] => score(held, goal)
				| (Discard c)::rest_held => aux(card_list, rest_held, remove_card(held, c, IllegalMove))
				| Draw ::rest => case card_list of
					[] => score(held, goal)
					| head::tail => aux(tail, rest, head::held)
	in
		aux(card_list, move_list, [])
	end


