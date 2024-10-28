// STGrammar.g4

grammar STGrammar;

// Parser Rules

// Top-Level Rule
compilation_unit
    : (namespace_declaration
      | type_declaration
      | interface_declaration
      | function
      | function_block
      | program
      | global_var_declarations
      )* EOF
    ;

// Program Structure
program
    : PROGRAM IDENTIFIER (var_block)* statement_list END_PROGRAM
    ;

// Function Declaration
function
    : FUNCTION IDENTIFIER COLON data_type (var_block)* statement_list END_FUNCTION
    ;

// Function Block Declaration
function_block
    : FUNCTION_BLOCK IDENTIFIER (var_block)* (method_declaration)* statement_list? END_FUNCTION_BLOCK
    ;

// Method Declarations
method_declaration
    : METHOD (PUBLIC | PRIVATE)? IDENTIFIER (COLON data_type)? (var_block)* statement_list END_METHOD
    ;

// Variable Declarations
var_block
    : (VAR | VAR_INPUT | VAR_OUTPUT | VAR_IN_OUT | VAR_TEMP) (variable_declaration)* END_VAR
    ;

variable_declaration
    : (PUBLIC | PRIVATE)? IDENTIFIER (COMMA IDENTIFIER)* COLON data_type (ASSIGN expression)? SEMI
    ;

// Global Variable Declarations
global_var_declarations
    : VAR_GLOBAL var_block END_VAR
    ;

// Data Types
data_type
    : BOOL | INT | REAL | STRING | VOID | array_type | struct_type | IDENTIFIER
    ;

// Array Type
array_type
    : ARRAY LBRACK array_subrange_list RBRACK OF data_type
    ;

array_subrange_list
    : array_subrange (COMMA array_subrange)*
    ;

array_subrange
    : constant_expression DOT_DOT constant_expression
    ;

// Structure Type
struct_type
    : STRUCT variable_declaration+ END_STRUCT
    ;

// Type Declarations
type_declaration
    : TYPE type_definition+ END_TYPE
    ;

type_definition
    : IDENTIFIER ASSIGN data_type SEMI
    ;

// Statements
statement_list
    : statement*
    ;

statement
    : assignment_statement
    | if_statement
    | case_statement
    | for_statement
    | while_statement
    | repeat_statement
    | exit_statement
    | return_statement
    | continue_statement
    | break_statement
    | expression_statement
    | try_statement
    | method_call_statement
    ;

// Assignment Statement
assignment_statement
    : variable ASSIGN expression SEMI
    ;

// Expression Statement
expression_statement
    : expression SEMI
    ;

// Method Call Statement
method_call_statement
    : variable LPAREN (expression (COMMA expression)*)? RPAREN SEMI
    ;

// If Statement
if_statement
    : IF expression THEN statement_list (ELSIF expression THEN statement_list)* (ELSE statement_list)? END_IF
    ;

// Case Statement
case_statement
    : CASE expression OF case_element+ (ELSE statement_list)? END_CASE
    ;

case_element
    : case_label_list COLON statement_list
    ;

case_label_list
    : case_label (COMMA case_label)*
    ;

case_label
    : constant_expression (DOT_DOT constant_expression)?
    ;

// For Statement
for_statement
    : FOR IDENTIFIER ASSIGN expression TO expression (BY expression)? DO statement_list END_FOR
    ;

// While Statement
while_statement
    : WHILE expression DO statement_list END_WHILE
    ;

// Repeat Statement
repeat_statement
    : REPEAT statement_list UNTIL expression SEMI END_REPEAT
    ;

// Try Statement
try_statement
    : TRY statement_list (CATCH IDENTIFIER statement_list)? (FINALLY statement_list)? END_TRY
    ;

// Control Statements
exit_statement
    : EXIT SEMI
    ;

return_statement
    : RETURN (expression)? SEMI
    ;

continue_statement
    : CONTINUE SEMI
    ;

break_statement
    : BREAK SEMI
    ;

// Expressions
expression
    : logical_expression
    ;

logical_expression
    : logical_expression OR logical_term
    | logical_term
    ;

logical_term
    : logical_term AND logical_factor
    | logical_factor
    ;

logical_factor
    : NOT logical_factor
    | relational_expression
    ;

relational_expression
    : arithmetic_expression (relational_operator arithmetic_expression)?
    ;

relational_operator
    : EQ
    | NEQ
    | LT
    | LTE
    | GT
    | GTE
    ;

arithmetic_expression
    : arithmetic_expression PLUS arithmetic_term
    | arithmetic_expression MINUS arithmetic_term
    | arithmetic_term
    ;

arithmetic_term
    : arithmetic_term MUL arithmetic_factor
    | arithmetic_term DIV arithmetic_factor
    | arithmetic_term MOD arithmetic_factor
    | arithmetic_factor
    ;

arithmetic_factor
    : MINUS arithmetic_factor
    | primary_expression
    ;

primary_expression
    : LPAREN expression RPAREN
    | function_call
    | variable
    | constant
    ;

function_call
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

// Variable
variable
    : IDENTIFIER (DOT IDENTIFIER)* (LBRACK expression RBRACK)*
    ;

// Constants
constant
    : NUMBER
    | REAL_NUMBER
    | STRING_LITERAL
    | BOOL_LITERAL
    | HEX_NUMBER
    | BIN_NUMBER
    ;

// Constant Expressions
constant_expression
    : constant
    ;

// Namespaces
namespace_declaration
    : NAMESPACE IDENTIFIER namespace_body* END_NAMESPACE
    ;

namespace_body
    : var_block
    | function
    | function_block
    | type_declaration
    | method_declaration
    ;

// Interfaces
interface_declaration
    : INTERFACE IDENTIFIER (method_prototype)* END_INTERFACE
    ;

method_prototype
    : METHOD IDENTIFIER (COLON data_type)? SEMI
    ;

// Lexer Rules

// Keywords
PROGRAM         : 'PROGRAM';
END_PROGRAM     : 'END_PROGRAM';
FUNCTION        : 'FUNCTION';
END_FUNCTION    : 'END_FUNCTION';
FUNCTION_BLOCK  : 'FUNCTION_BLOCK';
END_FUNCTION_BLOCK : 'END_FUNCTION_BLOCK';
METHOD          : 'METHOD';
END_METHOD      : 'END_METHOD';
VAR             : 'VAR';
VAR_INPUT       : 'VAR_INPUT';
VAR_OUTPUT      : 'VAR_OUTPUT';
VAR_IN_OUT      : 'VAR_IN_OUT';
VAR_TEMP        : 'VAR_TEMP';
VAR_GLOBAL      : 'VAR_GLOBAL';  // For global variables
END_VAR         : 'END_VAR';
TYPE            : 'TYPE';
END_TYPE        : 'END_TYPE';
STRUCT          : 'STRUCT';
END_STRUCT      : 'END_STRUCT';
ARRAY           : 'ARRAY';
OF              : 'OF';
BOOL            : 'BOOL';
INT             : 'INT';
REAL            : 'REAL';
STRING          : 'STRING';
VOID            : 'VOID';
IF              : 'IF';
THEN            : 'THEN';
ELSE            : 'ELSE';
ELSIF           : 'ELSIF';
END_IF          : 'END_IF';
CASE            : 'CASE';
END_CASE        : 'END_CASE';
FOR             : 'FOR';
TO              : 'TO';
BY              : 'BY';
DO              : 'DO';
END_FOR         : 'END_FOR';
WHILE           : 'WHILE';
END_WHILE       : 'END_WHILE';
REPEAT          : 'REPEAT';
UNTIL           : 'UNTIL';
END_REPEAT      : 'END_REPEAT';
EXIT            : 'EXIT';
RETURN          : 'RETURN';
CONTINUE        : 'CONTINUE';
BREAK           : 'BREAK';
TRY             : 'TRY';
CATCH           : 'CATCH';
FINALLY         : 'FINALLY';
END_TRY         : 'END_TRY';
NAMESPACE       : 'NAMESPACE';
END_NAMESPACE   : 'END_NAMESPACE';
INTERFACE       : 'INTERFACE';
END_INTERFACE   : 'END_INTERFACE';
PUBLIC          : 'PUBLIC';
PRIVATE         : 'PRIVATE';

// Operators and Punctuation
ASSIGN          : ':=';
SEMI            : ';';
COLON           : ':';
COMMA           : ',';
DOT             : '.';
DOT_DOT         : '..';
LPAREN          : '(';
RPAREN          : ')';
LBRACK          : '[';
RBRACK          : ']';

// Relational Operators
EQ              : '=';
NEQ             : '<>';
LT              : '<';
LTE             : '<=';
GT              : '>';
GTE             : '>=';

// Logical Operators
AND             : 'AND';
OR              : 'OR';
NOT             : 'NOT';
XOR             : 'XOR';

// Arithmetic Operators
PLUS            : '+';
MINUS           : '-';
MUL             : '*';
DIV             : '/';
MOD             : 'MOD';

// Literals
BOOL_LITERAL
    : 'TRUE' | 'FALSE'
    ;

NUMBER
    : DECIMAL_LITERAL
    ;

REAL_NUMBER
    : REAL_LITERAL
    ;

HEX_NUMBER
    : HEX_LITERAL
    ;

BIN_NUMBER
    : BIN_LITERAL
    ;

fragment DECIMAL_LITERAL
    : [0-9]+
    ;

fragment REAL_LITERAL
    : [0-9]+ '.' [0-9]+ (('E' | 'e') ('+' | '-')? [0-9]+)?
    ;

fragment HEX_LITERAL
    : '16#' [0-9A-Fa-f]+
    ;

fragment BIN_LITERAL
    : '2#' [01]+
    ;

STRING_LITERAL
    : '\'' ( ~('\'' | '\\') | '\\' . )* '\''
    ;

// Identifiers
IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

// Comments and Pragmas
LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

COMMENT
    : '(*' .*? '*)' -> skip
    ;

PRAGMA
    : '{' '#' .*? '#' '}' -> skip
    ;

// Whitespace
WS
    : [ \t\r\n]+ -> skip
    ;
