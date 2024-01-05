grammar ultra;

program *
    : (highLevelEntry ';;')+
    ;

highLevelEntry *
    : defExp *
    | infix_exp *
    ;

defExp *
    : defVal *
    | defFn *
    ;

defVal *
    : VAL identifier DEFEQ infix_exp *
    ;

defFn *
    : FN identifier argsExp DEFEQ infix_exp *
    ;

infix_exp *
    : exp (infix_op exp)? *
    ;

exp
    : ifExp *
    | LPAREN infix_exp RPAREN *
    | condExp *
    | letExp *
    | applyExp
    | lambdaDef
    | literal *
    | lit_list *
    | callExp *
    | identifier *
    ;

literal *
    : LIT_BOOL *
    | LIT_INT *
    | LIT_NIL *
    ;

ifExp *
    : IF infix_exp THEN infix_exp ELSE infix_exp *
    ;

condExp *
    : COND (COND_DELIM infix_exp COND_ARROW infix_exp)+
    ;

letExp *
    : LET identifier DEFEQ infix_exp IN infix_exp *
    ;

applyExp
    : APPLY identifier (infix_exp)+
    ;

callExp
    : identifier LPAREN infix_exp (',' infix_exp)* RPAREN
    ;

lambdaDef
    : LAM identifier (argsExp)? DEFEQ infix_exp
    ;

argsExp *
    : (identifier | freeIdentifier)+ | LIT_UNIT *
    ;

identifier *
    : IDENTIFIER *
    ;

infix_op *
    : PLUS
    | MINUS
    | PROD
    | DIV
    | PRIM_EQ
    | GT
    | LT
    ;

freeIdentifier
    : '@' IDENTIFIER;

lit_list *
    : LBRACKET infix_exp (COMMA infix_exp)* RBRACKET
    ;

WHITESPACE : [ \t\n]+ -> skip;

FN  : 'fn';
VAL : 'val';
LAM : 'lam' | 'lambda';

LET : 'let';
IN  : 'in';

IF   : 'if';
THEN : 'then';
ELSE : 'else';

COND       : 'cond';
COND_DELIM : '|';
COND_ARROW : '=>';

APPLY : 'apply';

DEFEQ : '=';

LPAREN : '(';
RPAREN : ')';
LBRACKET : '[';
RBRACKET : ']';

COMMA : ',';

SOBAKA : '@';
QUOTE  : '`';

LIT_BOOL : '#True' | '#False' | 'otherwise';
LIT_INT  : (MINUS)? '0'..'9'+;
LIT_NIL  : '[]';
LIT_UNIT : 'nothing';

MINUS : '-';
PLUS  :  '+';
PROD  :  '*';
DIV   :   '/';
GT    :   '>';
LT    :   '<';
PRIM_EQ : '==';

IDENTIFIER  : IDENT_START IDENT_CHAR*;
IDENT_START : 'a'..'z' | 'A'..'Z' | '_';
IDENT_CHAR  : IDENT_START | '0'..'9';