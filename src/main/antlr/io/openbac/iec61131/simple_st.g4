/*
simplified ST grammar
*/

grammar STGrammar;

// Parser rules
program
    : PROGRAM IDENTIFIER (var_block)* statement_list END_PROGRAM
    ;

var_block
    : VAR var_declaration+ END_VAR
    ;

var_declaration
    : IDENTIFIER (COMMA IDENTIFIER)* COLON data_type SEMI
    ;

data_type
    : IDENTIFIER
    ;

statement_list
    : statement*
    ;

statement
    : assignment_statement
    | if_statement
    | while_statement
    | for_statement
    | repeat_statement
    | case_statement
    | expression_statement
    ;

assignment_statement
    : IDENTIFIER ASSIGN expression SEMI
    ;

expression_statement
    : expression SEMI
    ;

if_statement
    : IF expression THEN statement_list (ELSIF expression THEN statement_list)* 
    (ELSE statement_list)? END_IF
    ;

while_statement
    : WHILE expression DO statement_list END_WHILE
    ;

for_statement
    : FOR IDENTIFIER ASSIGN expression TO expression (BY expression)? DO statement_list END_FOR
    ;

repeat_statement
    : REPEAT statement_list UNTIL expression SEMI END_REPEAT
    ;

case_statement
    : CASE expression OF case_element+ END_CASE
    ;

case_element
    : case_label_list COLON statement_list
    ;

case_label_list
    : case_label (COMMA case_label)*
    ;

case_label
    : expression
    ;

expression
    : simple_expression ((EQ | NEQ | LT | LTE | GT | GTE) simple_expression)?
    ;

simple_expression
    : term ((PLUS | MINUS | OR) term)*
    ;

term
    : factor ((MUL | DIV | MOD | AND) factor)*
    ;

factor
    : IDENTIFIER
    | NUMBER
    | LPAREN expression RPAREN
    | NOT factor
    | MINUS factor
    ;

// Lexer rules
PROGRAM     : 'PROGRAM';
END_PROGRAM : 'END_PROGRAM';
VAR         : 'VAR';
END_VAR     : 'END_VAR';
ASSIGN      : ':=';
SEMI        : ';';
COLON       : ':';
COMMA       : ',';
LPAREN      : '(';
RPAREN      : ')';
PLUS        : '+';
MINUS       : '-';
MUL         : '*';
DIV         : '/';
MOD         : 'MOD';
AND         : 'AND';
OR          : 'OR';
NOT         : 'NOT';
EQ          : '=';
NEQ         : '<>';
LT          : '<';
LTE         : '<=';
GT          : '>';
GTE         : '>=';
IF          : 'IF';
THEN        : 'THEN';
ELSE        : 'ELSE';
ELSIF       : 'ELSIF';
END_IF      : 'END_IF';
WHILE       : 'WHILE';
DO          : 'DO';
END_WHILE   : 'END_WHILE';
FOR         : 'FOR';
TO          : 'TO';
BY          : 'BY';
END_FOR     : 'END_FOR';
REPEAT      : 'REPEAT';
UNTIL       : 'UNTIL';
END_REPEAT  : 'END_REPEAT';
CASE        : 'CASE';
OF          : 'OF';
END_CASE    : 'END_CASE';

IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

NUMBER
    : [0-9]+
    ;

STRING_LITERAL
    : '\'' ( ~('\'' | '\\') | '\\' . )* '\''
    ;

COMMENT
    : '(*' .*? '*)' -> skip
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
