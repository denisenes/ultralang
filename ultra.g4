grammar ultra;

// {{ region Highlevel

program
    :
    (MODULE module_name BEGIN
        (IMPORTS LPAREN module_name (COMMA module_name)* RPAREN)?
        (EXPORTS LPAREN identifier (COMMA identifier)* RPAREN)?
        (hl_exp)+
    ENDMODULE)+
    ;

// TODO environment requirements

hl_exp
    : def_type
    | def_const
    | def_eval
    | def_fn
    | decl_fn
    ;

def_eval // used only in REPL, prohibited in source code
    : EVAL infix_exp
    ;

def_const
    : CONST identifier EQ infix_exp
    ;

def_fn
    : FN identifier args_exp (TYPE_DEL type_exp)? EQ infix_exp
    ;

decl_fn
    : DECL identifier TYPE_DEL type_exp
    ;

module_name
    : UPPER_IDENTIFIER
    ;

// endregion }}

// {{ region Types

type_exp
    : type_var
    | type_app
    ;

type_var
    : LOWER_IDENTIFIER
    ;

type_app
    : UPPER_IDENTIFIER (LBRACKET type_exp (COMMA type_exp)* RBRACKET)?
    ;

def_type
    : TYPE type_constructor EQ
        (COND_DELIM data_constructor)+
    ;

type_constructor
    : UPPER_IDENTIFIER (LBRACKET type_var (COMMA type_var)* RBRACKET)?
    ;

data_constructor
    : identifier (LPAREN identifier TYPE_DEL type_exp (COMMA identifier TYPE_DEL type_exp)* RPAREN)?
    ;

// endregion }}

// region Expresions

// TODO sequential let
// TODO environment bindings

infix_exp
    : exp (infix_op exp | TYPE_DEL type_exp)?
    ;

exp
    : seq_exp
    | if_exp
    | LPAREN infix_exp RPAREN
    | cond_exp
    | let_exp
    | lambda_exp
    | literal
    | lit_list
    | app_exp
    | identifier
    ;

literal
    : LIT_STR
    | LIT_INT
    | LIT_NIL
    | LIT_UNIT
    ;

seq_exp
    : SEQ (infix_exp ';')+ END
    ;

if_exp
    : IF infix_exp THEN infix_exp ELSE infix_exp
    ;

cond_exp
    : COND (COND_DELIM infix_exp COND_ARROW infix_exp)+ ENDCOND
    ;

let_exp
    : LET identifier EQ infix_exp IN infix_exp
    ;

app_exp
    : identifier LPAREN (infix_exp (',' infix_exp)*)? RPAREN
    ;

lambda_exp
    : FN args_exp (TYPE_DEL type_exp)? EQ infix_exp
    ;

args_exp
    : LPAREN (identifier (',' identifier)*)? RPAREN
    ;

// TODO qualified identifiers

identifier
    : LOWER_IDENTIFIER
    ;

infix_op
    : (MINUS | PLUS | PROD | DIV | GT | LT | EQ)+
    ;

lit_list
    : LBRACKET infix_exp (COMMA infix_exp)* RBRACKET
    ;

// endregion }}

// {{ region Tokens

WHITESPACE : [ \t\n]+ -> skip;

MODULE : 'module';
BEGIN : 'begin';
ENDMODULE : 'endmodule';
IMPORTS : 'imports';
EXPORTS : 'exports';

EVAL : 'eval';

DECL  : 'decl';
FN    : 'fn';
CONST : 'const';

SEQ : 'seq';
END : 'end';

LET : 'let';
IN  : 'in';

IF   : 'if';
THEN : 'then';
ELSE : 'else';

COND       : 'cond';
ENDCOND    : 'endcond';
COND_DELIM : '|';
COND_ARROW : '=>';

APPLY : 'apply';

LPAREN : '(';
RPAREN : ')';
LBRACKET : '[';
RBRACKET : ']';

COMMA : ',';

SOBAKA : '@';
QUOTE  : '`';

LIT_INT  : (MINUS)? '0'..'9'+;
LIT_NIL  : '[]';
LIT_STR  : '"' ~('\r' | '\n' | '"')* '"' ;
LIT_UNIT : 'unit';

EQ    : '=';
MINUS : '-';
PLUS  : '+';
PROD  : '*';
DIV   : '/';
GT    : '>';
LT    : '<';

TYPE_DEL : ':';
TYPE : 'type';

LOWER_IDENTIFIER  : LOWER_IDENTIFIER_START LOWER_IDENTIFIER_CHAR*;
LOWER_IDENTIFIER_START : 'a'..'z' | '_';
LOWER_IDENTIFIER_CHAR  : LOWER_IDENTIFIER_START | 'A'..'Z' | '0'..'9';

UPPER_IDENTIFIER  : UPPER_IDENTIFIER_START UPPER_IDENTIFIER_CHAR*;
UPPER_IDENTIFIER_START : 'A'..'Z';
UPPER_IDENTIFIER_CHAR  : UPPER_IDENTIFIER_START | 'a'..'z';

// endregion }}
