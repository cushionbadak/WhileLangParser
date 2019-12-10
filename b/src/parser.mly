%{
  open BLang
%}

/*T_LANGLEBRACKET is "<", so is T_LT*/
%token T_TRUE
%token T_FALSE
%token T_NOT
%token T_IF
%token T_THEN
%token T_ELSE
%token T_WHILE
%token T_LET
%token T_PROC
%token T_IN

%token<int>     T_NUM
%token<string>  T_STR

%token T_LPAREN
%token T_RPAREN
%token T_LBRACKET
%token T_RBRACKET
%token T_ADD
%token T_SUB
%token T_MUL
%token T_DIV
%token T_ASSIGN
%token T_EQ
%token T_LT
%token T_GT 
%token T_HASH
%token T_COMMA
%token T_SEMICOLON
%token T_DOT
%token EOF


/* associativity settings*/

%right T_NOT
%left T_SEMICOLON
%left T_ELSE
%left T_ADD T_SUB
%left T_MUL T_DIV


%start programlist
%type <BLang.program list> programlist

%%

programlist:
  | program T_HASH programlist {$1::$3}
  | program EOF {[$1]}
  | EOF {[]}
;

program:
  stmt {$1}
;

stmt: 
  | T_LET T_PROC T_STR T_LPAREN args T_RPAREN T_EQ stmt T_IN stmt { LETF ($3,$5,$8,$10) }
  | T_LET T_PROC T_STR T_LPAREN T_RPAREN T_EQ stmt T_IN stmt { LETF ($3,[],$7,$9) }
  | T_LET T_STR T_EQ stmt T_IN stmt { LETV ($2,$4,$6) }
  | T_WHILE T_LPAREN exp0 T_RPAREN T_LBRACKET stmt T_RBRACKET { WHILE ($3,$6) }
  | T_IF exp0 T_THEN stmt T_ELSE stmt { IF ($2,$4,$6) }
  | exp0 {$1}

exp0:
  | stmt T_SEMICOLON stmt { SEQ ($1,$3) }
  | exp1 {$1}
;    

exp1:
  | T_STR T_ASSIGN exp1 { ASSIGN ($1,$3) }
  | exp5 T_DOT T_STR T_ASSIGN exp1 { ASSIGNF ($1,$3,$5) }
  | exp1 T_EQ exp1 { EQUAL($1,$3) }
  | exp1 T_LT exp1 { LESS($1,$3) }
  | T_NOT exp1 { NOT($2) }
  | exp2 {$1}
;

exp2:
  | T_SUB T_NUM { NUM ((-1)*($2)) }
  | exp3 {$1} 
;

exp3:
  | exp3 T_ADD exp3 { ADD($1,$3) }
  | exp3 T_SUB exp3 { SUB($1,$3) }
  | exp4 {$1}
;

exp4:
  | exp4 T_MUL exp4 { MUL($1,$3) }
  | exp4 T_DIV exp4 { DIV($1,$3) }
  | exp5 {$1}
;
  
exp5:
  | T_STR T_LT args T_GT { CALLR($1,$3) }
  | T_STR T_LT T_GT { CALLR($1,[]) }
  | T_STR T_LPAREN exps T_RPAREN { CALLV($1,$3) }
  | T_STR T_LPAREN T_RPAREN { CALLV($1,[]) }
  | exp5 T_DOT T_STR { FIELD($1,$3) }
  | exp6 {$1}
;

args:
  | T_STR T_COMMA args {$1::$3}
  | T_STR {[$1]}
;

exps:
  | exp0 T_COMMA exps {$1::$3}
  | exp0 {[$1]}
;

exp6:
  | T_LPAREN T_RPAREN { UNIT }
  | T_LPAREN stmt T_RPAREN { $2 }
  | T_TRUE { TRUE }
  | T_FALSE { FALSE }
  | T_NUM { NUM($1) }
  | T_STR { VAR($1) }
  | T_LBRACKET T_RBRACKET { RECORD ([]) }
  | T_LBRACKET binds T_RBRACKET { RECORD ($2) }
;

binds:
  | T_STR T_ASSIGN exp1 T_COMMA binds { ($1,$3)::$5 }
  | T_STR T_ASSIGN exp1 { [($1,$3)] }
;
