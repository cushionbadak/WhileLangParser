(* operator precedence
0. Unique Values ( 3, -2, true, false, (), )
1. Parenthesis 
  - ( (...) )
2. call
3. Arithmetic Operators
  - ( *, /)
  - ( +, -)
4. List Operators
5. Boolean Operators
  - ( =, <)
6. print
7. seq
8. proc
9. let-in, letrec-in, letrec-and-in
10. (HASHTAG symbol '#' for delimeter)
*)

exception UndefinedSemantics
exception NotImplemented

type program = exp
and exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assign to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(***************************)
(*     pretty printer      *)
(***************************)

let rec prog2str : exp -> string
= fun e ->
  match e with
  | NUM n -> "NUM "^string_of_int n
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | UNIT -> "UNIT"
  | VAR x -> "VAR \""^x^"\""
  | ADD (e1,e2) -> "ADD ("^prog2str e1^", "^prog2str e2^")"
  | SUB (e1,e2) -> "SUB ("^prog2str e1^", "^prog2str e2^")"
  | MUL (e1,e2) -> "MUL ("^prog2str e1^", "^prog2str e2^")"
  | DIV (e1,e2) -> "DIV ("^prog2str e1^", "^prog2str e2^")"
  | EQUAL (e1,e2) -> "EQUAL ("^prog2str e1^", "^prog2str e2^")"
  | LESS (e1,e2) -> "LESS ("^prog2str e1^", "^prog2str e2^")"
  | NOT e -> "NOT ("^prog2str e^")"
  | SEQ (e1,e2) -> "SEQ ("^prog2str e1^ ", " ^ prog2str e2 ^")"
  | IF (e1,e2,e3) -> "IF (" ^prog2str e1^ ", "^prog2str e2 ^", "^prog2str e3 ^")"
  | WHILE (e1,e2) -> "WHILE ("^prog2str e1^ ", "^prog2str e2^")"
  | LETV (f,e1,e2) -> "LETV (\""^f^ "\", "^prog2str e1^", "^prog2str e2^")"
  | LETF (f,args,e1,e2) -> "LETF (\""^f^"\", "^"[ "^args2str args^" ],"^ prog2str e1^","^prog2str e2^")"
  | CALLV (f, es) -> "CALLV ("^"\""^f^"\""^ ", ["^exps2str es^"])"
  | CALLR (f, ys) -> "CALLR ("^"\""^f^"\""^ ", ["^args2str ys^"])"
  | RECORD binds -> "RECORD (["^ record2str binds^"])"
  | FIELD (exp,id) -> "FIELD ("^ prog2str exp^", \""^id^"\")"
  | ASSIGN (id,e) -> "ASSIGN ("^ "\"" ^ id ^ "\"" ^ ", "^ prog2str e^")"
  | ASSIGNF (e1,x,e2) ->"ASSIGNF ("^ prog2str e1^", "^ "\"" ^x^ "\""^", "^prog2str e2^")"
  | WRITE e -> "WRITE ("^prog2str e^")"

and record2str : (id*exp) list -> string
= fun binds ->
  match binds with
  | [] -> ""
  | (id,exp)::t -> "("^"\""^id^"\""^", "^prog2str exp^"); "^record2str t

and args2str : id list -> string
= fun xs ->
  match xs with
  | [] -> ""
  | h::t -> "\""^h^"\""^ "; " ^ args2str t

and exps2str : exp list -> string
= fun es ->
  match es with
  | [] -> ""
  | h::t -> prog2str h^"; " ^ exps2str t

(***************************)
