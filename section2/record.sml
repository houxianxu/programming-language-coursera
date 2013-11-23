val x = {bar = (1+2, true andalso true), foo = 3+4, baz =(false, 9)};
val brain_pat = {id = 1111, ego = false, superego = false};
#id brain_pat;


(* tuples are just "syntactic sugar" for records*)
val x = {3="hi", 1 = true};

val y = {3="hi", 1 =true, 2 = "343"};

(*
Syntactic: Can descibe the semantics entirely by the corresponding record syntax
Sugar: They make the language sweeter
*)

(*result*)
val y = (true,"343","hi") : bool * string * string
val x = {bar=(3,true),baz=(false,9),foo=7}
  : {bar:int * bool, baz:bool * int, foo:int}
val brain_pat = {e go=false,id=1111,superego=false}
  : {ego:bool, id:int, superego:bool}
val it = 1111 : int
val x = {1=true,3="hi"} : {1:bool, 3:string}
val y = (true,"343","hi") : bool * string * string
- 