(* operator precedence
0. Unique Values ( 3, -2, true, false, (), )
1. Parenthesis 
  - ( (...) )
2. call
3. Arithmetic Operators
  - ( *, /)
  - ( +, -)
4. List Operators
  - ( ::)
  - ( @)
5. Boolean Operators
  - ( =, <)
6. print
7. seq
8. proc
9. let-in, letrec-in, letrec-and-in
10. (HASHTAG symbol '#' for delimeter)
*)

type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of (var * var * exp) * (var * var * exp) * env
and env = (var * value) list

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ List.fold_left (fun s x -> s ^ ", " ^ x) "" (List.map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let rec ocaml_string_of_program p = 
  let osop = ocaml_string_of_program in
  let unary s e = s ^ " (" ^ (osop e) ^ ")" in
  let binary s e1 e2 = s ^ " (" ^ (osop e1) ^ ", " ^ (osop e2) ^ ")" in
  match p with
  | UNIT -> "UNIT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | CONST n -> "CONST " ^ (if (n >= 0) then (string_of_int n) else ("(" ^ (string_of_int n) ^ ")"))
  | VAR v -> "VAR (\"" ^ v ^ "\")"
  | ADD (e1, e2) -> binary "ADD" e1 e2
  | SUB (e1, e2) -> binary "SUB" e1 e2
  | MUL (e1, e2) -> binary "MUL" e1 e2
  | DIV (e1, e2) -> binary "DIV" e1 e2
  | EQUAL (e1, e2) -> binary "EQUAL" e1 e2
  | LESS (e1, e2) -> binary "LESS" e1 e2
  | NOT e -> unary "NOT" e
  | NIL -> "NIL"
  | CONS (e1, e2) -> binary "CONS" e1 e2
  | APPEND (e1, e2) -> binary "APPEND" e1 e2
  | HEAD e -> unary "HEAD" e
  | TAIL e -> unary "TAIL" e
  | ISNIL e -> unary "ISNIL" e
  | IF (e1, e2, e3) ->
      "IF (" ^ (osop e1) ^ ", " ^ (osop e2) ^ ", " ^ (osop e3) ^ ")"
  | LET (v, e1, e2) ->
      "LET (\"" ^ v ^ "\", " ^ (osop e1) ^ ", " ^ (osop e2) ^ ")"
  | LETREC (v1, v2, e1, e2) ->
      "LETREC (\"" ^ v1 ^ "\", \"" ^ v2 ^ "\", " ^ (osop e1) ^ ", " ^ (osop e2) ^ ")"
  | LETMREC ((v1, v2, e1), (v3, v4, e2), e3) ->
      "LETMREC ((\"" ^ v1 ^ "\", \"" ^ v2 ^ "\", " ^ (osop e1) ^ "), (\"" ^ v3 ^ "\", \"" ^ v4 ^ "\", " ^ (osop e2) ^ "), " ^ (osop e3) ^ ")"
  | PROC (v, e) -> "PROC (\"" ^ v ^ "\", " ^ (osop e) ^ ")"
  | CALL (e1, e2) -> binary "CALL" e1 e2
  | PRINT e -> unary "PRINT" e
  | SEQ (e1, e2) -> binary "SEQ" e1 e2


exception UndefinedSemantics

