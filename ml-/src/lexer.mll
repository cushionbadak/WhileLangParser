{
open Parser
exception LexicalError
let reserved_word = Hashtbl.create 31
let _ = List.iter (fun (keyword, tok) -> Hashtbl.add reserved_word keyword tok)
  [
    ("true", T_TRUE);
    ("false", T_FALSE);
    ("not", T_NOT);
    ("nil", T_NIL);
    ("cons", T_CONS);
    ("head", T_HEAD);
    ("tail", T_TAIL);
    ("isnil", T_ISNIL);
    ("if", T_IF);
    ("then", T_THEN);
    ("else", T_ELSE);
    ("let", T_LET);
    ("in", T_IN);
    ("letrec", T_LETREC);
    ("and", T_AND);
    ("proc", T_PROC);
    ("print", T_PRINT);
  ]
}


let numeral = '0' | ['1'-'9']['0'-'9']*
let blank = [' ' '\n' '\t' '\r']+
let word = ['a'-'z' 'A'-'Z' '_']*

rule start = parse
  | blank     { start lexbuf }
  | "("       { T_LPARENSYM }
  | ")"       { T_RPARENSYM }
  | "+"       { T_ADDSYM }
  | "-"       { T_SUBSYM }
  | "*"       { T_MULSYM }
  | "/"       { T_DIVSYM }
  | "="       { T_EQSYM }
  | "<"       { T_LTSYM }
  | "::"      { T_CONSSYM }
  | "@"       { T_APPENDSYM }
  | ";"       { T_SCOLONSYM }
  | "#"       { T_HASHSYM }
  | ","       { T_COMMASYM }
  | "["       { T_LBRACKETSYM }
  | "]"       { T_RBRACKETSYM }
  | numeral   { T_NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | word      { let word = Lexing.lexeme lexbuf in
                  try
                    Hashtbl.find reserved_word word
                  with _ -> T_STR word
              }
  | eof       { EOF }
  | _         { raise LexicalError }

