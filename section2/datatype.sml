datatype mytype = 
    TowInts of int * int 
  | Str of string
  | Pizza

(* mytype -> int *)
fun f (x: mytype) =
	case x of
	    Pizza => 3
	  | Str s => 8
	  | TowInts(i1, i2) => i1 + i2

(* Useful Datatypes *)
datatype suit = Club | Diamond | Heart | Spade 

datatype exp = 
    Constant  of int 
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

fun eval e = 
	case e of
	    Constant i => i
	  | Negate e2  => ~ (eval e2)
	  | Add(e1, e2) => (eval e1) + (eval e2)
	  | Multiply(e1, e2) => (eval e1) * (eval e2)

val example_exp = Add (Constant 19, Negate(Constant 4))

val example_ans = eval example_exp

fun max_constant e =
	
    case e of
	    Constant i => i
	  | Negate e2 => max_constant e2
	  | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
	  | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)
	
	

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
    

