%{
  open WhileLang
%}

%token T_LPARENSYM
%token T_RPARENSYM

/* NOTICE: "T_LBRACKETSYM" and "T_RBRACKETSYM" are not defined in this language, but they are supported for homework-example compatibility.*/
%token T_LBRACKETSYM
%token T_RBRACKETSYM

%token T_TRUE
%token T_FALSE

%token<int>     T_NUM
%token<string>  T_STR

%token T_ADDSYM
%token T_SUBSYM
%token T_MULSYM
%token T_DIVSYM

%token T_EQSYM
%token T_LTSYM

%token T_NOT

/* NOTICE: "T_CONS" is not defined in this language, but it is supported for homework-example compatibility. */
%token T_NIL
%token T_CONS
%token T_CONSSYM
%token T_APPENDSYM
%token T_HEAD
%token T_TAIL
%token T_ISNIL

%token T_IF
%token T_THEN
%token T_ELSE

%token T_LET
%token T_IN
%token T_LETREC
%token T_AND
%token T_PROC
%token T_PRINT
%token T_SCOLONSYM

/* NOTICE: "T_HASHSYM" is not defined in this language, but it is supported for easy testcase generation. */
%token T_HASHSYM
/* NOTICE: "T_COMMASYM" is not defined in this language, but it is supported for homework-example compatibility. */
%token T_COMMASYM
%token EOF

%start programlist
%type <WhileLang.program list> programlist

%%

programlist:
  | program T_HASHSYM programlist {$1::$3}
  | program EOF {[$1]}
  | EOF {[]}
;

program:
  exp0 {$1}
;

exp0:
  | T_LET T_STR T_EQSYM exp0 T_IN exp0 { LET($2,$4,$6) }
  | T_LETREC T_STR T_STR T_EQSYM exp0 T_IN exp0 { LETREC($2,$3,$5,$7) }
  | T_LETREC T_STR T_LPARENSYM T_STR T_RPARENSYM T_EQSYM exp0 T_IN exp0 { LETREC($2,$4,$7,$9) }
  | T_LETREC T_STR T_STR T_EQSYM exp0 T_AND T_STR T_STR T_EQSYM exp0 T_IN exp0 { LETMREC(($2,$3,$5), ($7,$8,$10), $12) }
  | T_LETREC T_STR T_LPARENSYM T_STR T_RPARENSYM T_EQSYM exp0 T_AND T_STR T_STR T_EQSYM exp0 T_IN exp0 { LETMREC(($2,$4,$7), ($9,$10,$12), $14) }
  | T_LETREC T_STR T_STR T_EQSYM exp0 T_AND T_STR T_LPARENSYM T_STR T_RPARENSYM T_EQSYM exp0 T_IN exp0 { LETMREC(($2,$3,$5), ($7,$9,$12), $14) }
  | T_LETREC T_STR T_LPARENSYM T_STR T_RPARENSYM T_EQSYM exp0 T_AND T_STR T_LPARENSYM T_STR T_RPARENSYM T_EQSYM exp0 T_IN exp0 { LETMREC(($2,$4,$7), ($9,$11,$14), $16) }
  | exp1 {$1}
;

exp1:
  | T_PROC T_LPARENSYM T_STR T_RPARENSYM exp1 { PROC($3,$5) }
  | exp2 {$1}
;

exp2:
  | exp2 T_SCOLONSYM exp3 { SEQ($1,$3) }
  | exp3 {$1}
;

exp3:
  | T_PRINT exp5 { PRINT($2) }
  | exp5 {$1}
;

exp5:
  | exp5 T_EQSYM exp66 { EQUAL($1,$3) }
  | exp5 T_LTSYM exp66 { LESS($1,$3) }
  | T_NOT exp66 { NOT($2) }
  | T_IF exp5 T_THEN exp5 T_ELSE exp66 { IF($2,$4,$6) }
  | exp66 {$1}
;

exp66:
  | exp66 T_APPENDSYM exp6 { APPEND($1,$3) }
  | exp6 {$1}

exp6:
  | T_NIL { NIL }
  | T_CONS T_LPARENSYM exp6 T_COMMASYM exp6 T_RPARENSYM { CONS($3,$5) }
  | T_LPARENSYM T_CONS exp6 T_NIL T_RPARENSYM { CONS($3,NIL) }
  | exp6 T_CONSSYM exp7 { CONS($1,$3) }
  | T_HEAD exp7 { HEAD($2) }
  | T_TAIL exp7 { TAIL($2) }
  | T_ISNIL exp7 { ISNIL($2) }
  | T_SUBSYM T_NUM { CONST ((-1)*($2)) }
  | exp7 {$1} 
;

exp7:
  | exp7 T_ADDSYM exp8 { ADD($1,$3) }
  | exp7 T_SUBSYM exp8 { SUB($1,$3) }
  | exp8 {$1}
;

exp8:
  | exp8 T_MULSYM exp9 { MUL($1,$3) }
  | exp8 T_DIVSYM exp9 { DIV($1,$3) }
  | exp9 {$1}
;

exp9:
  | exp9 exp10 { CALL($1,$2) }
  | exp10 {$1}
;

exp10:
  | T_LPARENSYM T_RPARENSYM { UNIT }
  | T_LPARENSYM exp0 T_RPARENSYM { $2 }
  | T_TRUE { TRUE }
  | T_FALSE { FALSE }
  | T_NUM { CONST($1) }
  | T_STR { VAR($1) }
  | T_LBRACKETSYM T_RBRACKETSYM { NIL }
;
