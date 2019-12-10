{
open Parser
exception LexicalError
let reserved_word = Hashtbl.create 31
let _ = List.iter (fun (keyword, tok) -> Hashtbl.add reserved_word keyword tok)
  [
    ("true", T_TRUE);
    ("false", T_FALSE);
    ("not", T_NOT);
    ("if", T_IF);
    ("then", T_THEN);
    ("else", T_ELSE);
    ("while", T_WHILE);
    ("let", T_LET);
    ("proc", T_PROC);
    ("in", T_IN);
  ]
}


let numeral = '0' | ['1'-'9']['0'-'9']*
let blank = [' ' '\n' '\t' '\r']+
let word = ['0'-'9' 'a'-'z' 'A'-'Z' '_' ]*

rule start = parse
  | blank     { start lexbuf }
  | "("       { T_LPAREN }
  | ")"       { T_RPAREN }
  | "{"       { T_LBRACKET }
  | "}"       { T_RBRACKET }
  | "+"       { T_ADD }
  | "-"       { T_SUB }
  | "*"       { T_MUL }
  | "/"       { T_DIV }
  | ":="      { T_ASSIGN }
  | "="       { T_EQ }
  | "<"       { T_LT }
  | ">"       { T_GT }
  | "#"       { T_HASH }
  | ","       { T_COMMA }
  | ";"       { T_SEMICOLON } 
  | "."       { T_DOT }
  | numeral   { T_NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | word      { let word = Lexing.lexeme lexbuf in
                  try
                    Hashtbl.find reserved_word word
                  with _ -> T_STR word
              }
  | eof       { EOF }
  | _         { raise LexicalError }

