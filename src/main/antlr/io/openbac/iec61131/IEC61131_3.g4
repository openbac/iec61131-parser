/*
	IEC61131 Grammar
    Copyright (C) 2018 Joerg Seitter
  
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

grammar IEC61131_3;


// --- library ---
// Top Level Entry Point
library_element_name 
	: data_type_name 
	| function_name 
	| function_block_type_name 
	| program_type_name
	| resource_type_name 
	| configuration_name
	;
	
program_type_name : IDENTIFIER;

configuration_name : IDENTIFIER;

resource_type_name : IDENTIFIER;


library_element_declaration 
	: data_type_declaration
	| function_declaration 
	| function_block_declaration 
	| program_declaration 
	| configuration_declaration
	;

// -- program

program_declaration : 'PROGRAM' program_type_name ( io_var_declarations | other_var_declarations | located_var_declarations | program_access_decls )* function_block_body 'END_PROGRAM';

program_access_decls : 'VAR_ACCESS' program_access_decl ';' (program_access_decl ';' )* 'END_VAR';

program_access_decl : access_name ':' symbolic_variable ':' non_generic_type_name (direction)?;

access_declarations : 'VAR_ACCESS'

access_declaration ';' (access_declaration ';')* 'END_VAR';

access_declaration : access_name ':' access_path ':' non_generic_type_name (direction)?;

access_path : (resource_name '.')? direct_variable | (resource_name '.')? (program_name '.')? (fb_name'.')* symbolic_variable;

global_var_reference : (resource_name '.')? global_var_name ('.' structure_element_name)?;

access_name : IDENTIFIER;

program_output_reference : program_name '.' symbolic_variable;

direction : 'READ_WRITE' | 'READ_ONLY';

program_name : IDENTIFIER;

// -- configuration

configuration_declaration : 'CONFIGURATION' configuration_name (global_var_declarations)* (single_resource_declaration | (resource_declaration (resource_declaration)*)) (access_declarations)? (instance_specific_initializations)? 'END_CONFIGURATION'; 

resource_declaration : 'RESOURCE' resource_name 'ON' resource_type_name (global_var_declarations)? single_resource_declaration 'END_RESOURCE'; single_resource_declaration : (task_configuration ';')* program_configuration ';' (program_configuration ';')*;

resource_name : IDENTIFIER;

task_configuration : 'TASK' task_name task_initialization;

task_name : IDENTIFIER;

task_initialization : '(' ('SINGLE' ':=' data_source ',')? ('INTERVAL' ':=' data_source ',')? 'PRIORITY' ':=' INTEGER ')';

data_source : constant | global_var_reference | program_output_reference | direct_variable;

program_configuration : 'PROGRAM' ('RETAIN' | 'NON_RETAIN')? program_name ('WITH' task_name)? ':' program_type_name ('(' prog_conf_elements ')')?;

prog_conf_elements : prog_conf_element (',' prog_conf_element)*;

prog_conf_element : fb_task | prog_cnxn;

fb_task : fb_name 'WITH' task_name;

prog_cnxn : symbolic_variable ':=' prog_data_source | symbolic_variable '=>' data_sink;

prog_data_source : constant | enumerated_value | global_var_reference | direct_variable;

data_sink : global_var_reference | direct_variable;

instance_specific_initializations : 'VAR_CONFIG' instance_specific_init ';' (instance_specific_init ';')* 'END_VAR';

instance_specific_init : resource_name '.' program_name '.' (fb_name '.')* ((variable_name (location)? ':' located_var_spec_init) | (fb_name ':' function_block_type_name ':=' structure_initialization));



// --- Expression Handling ---
expression 
    : xor_expression (OR xor_expression)*;

xor_expression 
    : and_expression (XOR and_expression)*;

and_expression 
    : comparison (AND comparison)*;

comparison 
    : equ_expression ( (EQU | NEQU) equ_expression)*;

equ_expression 
        : add_expression (comparison_operator add_expression)*;

comparison_operator 
        : LT 
        | GT 
        | LE 
        | GE
        ;

add_expression 
        : term (add_operator term)*
        ;

add_operator 
        : PLUS 
        | MINUS
        ;

term 
    : power_expression (multiply_operator power_expression)*;

multiply_operator 
        : MUL 
        | DIV 
        | MOD
        ;

power_expression 
        : unary_expression (POW unary_expression)*;

unary_expression 
        : (unary_operator)? primary_expression;

unary_operator 
        : MINUS 
        | NOT
        ;

primary_expression 
		: constant 
//		| enumerated_value
		| variable
		| LPAR expression RPAR
		| function_name LPAR param_assignment (',' param_assignment)* RPAR
		;

constant 
		: numeric_literal 
		| character_string 
		| time_literal 
		| bit_string_literal 
		| boolean_literal
		;

// --- literals ---


numeric_literal 
		: integer_literal 
		| real_literal
		;

real_literal 
	: ( real_type_name '#' )? FIXED_POINT (EXPONENT)?;


integer_literal 
		: (integer_type_name '#' )? (INTEGER | SIGNED_INTEGER | BINARY_INTEGER | OCTAL_INTEGER | HEX_INTEGER)
		;


data_type_name 
	: non_generic_type_name 
	| generic_type_name
	;

non_generic_type_name 
	: elementary_type_name 
	| derived_type_name;
	
derived_type_name 
	: single_element_type_name 
	| array_type_name 
	| structure_type_name 
	| string_type_name;

single_element_type_name 
	: simple_type_name 
	| subrange_type_name 
	| enumerated_type_name;

simple_type_name 
	: IDENTIFIER
	;

subrange_type_name 
	: IDENTIFIER
	;

enumerated_type_name 
	: IDENTIFIER
	;

array_type_name 
	: IDENTIFIER
	;

structure_type_name 
	: IDENTIFIER
	;
	
string_type_name 
	: IDENTIFIER
	;

elementary_type_name 
	: numeric_type_name 
	| date_type_name 
	| bit_string_type_name 
	| STRING
	| WSTRING
	;

numeric_type_name 
	: integer_type_name 
	| real_type_name;

integer_type_name 
	: signed_integer_type_name 
	| unsigned_integer_type_name
	;

signed_integer_type_name 
	: SINT 
	| INT 
	| DINT 
	| LINT
	;

unsigned_integer_type_name 
	: USINT 
	| UINT 
	| UDINT 
	| ULINT
	;

real_type_name 
	: REAL 
	| LREAL
	;

date_type_name 
	: DATE 
	| TIMEOFDAY
	| DATETIME
	;
	
bit_string_type_name 
	: BITSTRING | BOOL
	;

generic_type_name :
	GENERIC
	;


character_string 
	: single_byte_character_string 
	| double_byte_character_string
	;

single_byte_character_string 
//	: '\'' (single_byte_character_representation)* '\''
	:	SB_STRING_LITERAL
	;

double_byte_character_string 
//	: '"' (double_byte_character_representation)* '"'
	:	DB_STRING_LITERAL
	;
	
//single_byte_character_representation 
//	: common_character_representation 
//	| '$\'' 
//	| '"' 
//	| '$' HEX_BYTE
//	;

//double_byte_character_representation 
//	: common_character_representation 
//	| '$"' 
//	| '\'' 
//	| '$' HEX_WORD
//	;

//common_character_representation : <any printable character except '$', '"' or "'"> | '$$' | '$L' | '$N' | '$P' | '$R' | '$T' | '$l' | '$n' | '$p' | '$r' | '$t'
//common_character_representation 
//	: STRING_CHARACTERS
//	| '$$' 
//	| '$L' 
//	| '$N' 
//	| '$P' 
//	| '$R' 
//	| '$T' 
//	| '$l' 
//	| '$n' 
//	| '$p' 
//	| '$r' 
//	| '$t' 
//	;

boolean_literal 
	: ( BOOL '#' )? BOOLEAN 
	;
	
bit_string_literal 
	: ( (BITSTRING) '#' )?( INTEGER | BINARY_INTEGER | OCTAL_INTEGER | HEX_INTEGER)
	;


time_literal 
	: duration 
	| time_of_day
	;

duration 
	: TIME '#' (MINUS)? interval;

interval 
	: DAYS 
	| HOURS 
	| MINUTES 
	| SECONDS 
	| MILLISECONDS
	;

time_of_day : TIMEOFDAY '#' daytime;

daytime : day_hour COLON day_minute COLON day_second;

day_hour : INTEGER;

day_minute : INTEGER;

day_second : FIXED_POINT;

date : DATE '#' date_literal;

date_literal : year MINUS month MINUS day;

year : INTEGER;

month : INTEGER;

day : INTEGER;

date_and_time : DATETIME '#' date_literal MINUS daytime;

// --- enums ---
//enumerated_value : (enumerated_type_name '#')? IDENTIFIER;

//enumerated_type_name 
//	: IDENTIFIER
//	;

// --- declared types

data_type_declaration 
	: 'TYPE' type_declaration ';' (type_declaration ';')* 'END_TYPE'
	;

type_declaration 
	: single_element_type_declaration 
	| array_type_declaration 
	| structure_type_declaration 
	| string_type_declaration
	;
	
single_element_type_declaration 
	: simple_type_declaration 
	| subrange_type_declaration 
	| enumerated_type_declaration
	;

simple_type_declaration 
	: simple_type_name ':' simple_spec_init
	;

simple_spec_init 
	: simple_specification (':=' constant)?
	;

simple_specification 
	: elementary_type_name 
	| simple_type_name
	;

// --- function blocks ---

function_block_declaration 
	: 'FUNCTION_BLOCK' derived_function_block_name ( io_var_declarations | other_var_declarations )* function_block_body 'END_FUNCTION_BLOCK'
	;

other_var_declarations 
	: external_var_declarations 
	| var_declarations 
	| retentive_var_declarations 
	| non_retentive_var_declarations 
	| temp_var_decls 
	| incompl_located_var_declarations
	;

temp_var_decls 
	: 'VAR_TEMP' temp_var_decl ';' (temp_var_decl ';')* 'END_VAR'
	;

function_block_body 
	: statement_list 
	;

// --- functions ---

function_body 
	:  statement_list 
	;

function_declaration 
	: 'FUNCTION' derived_function_name ':' (elementary_type_name | derived_type_name)  io_var_declarations | function_var_decls  function_body 'END_FUNCTION';

io_var_declarations 
	: input_declarations 
	| output_declarations 
	| input_output_declarations
	;
	
input_declarations 
	: 'VAR_INPUT' ('RETAIN' | 'NON_RETAIN')? input_declaration ';' (input_declaration ';')* 'END_VAR'
	;

input_declaration 
	: var_init_decl 
	| edge_declaration
	;
	
// TODO review the tokens and move down to lexer
edge_declaration 
	: var1_list ':' BOOL ('R_EDGE' | 'F_EDGE')
	;

var_init_decl 
	: var1_init_decl 
	| array_var_init_decl 
	| structured_var_init_decl 
	| fb_name_decl 
	| string_var_declaration
	;

var1_init_decl 
	: var1_list ':' (simple_spec_init | subrange_spec_init | enumerated_spec_init)
	;

var1_list 
	: variable_name (',' variable_name)*
	;
	
array_var_init_decl 
	: var1_list ':' array_spec_init
	;

structured_var_init_decl 
	: var1_list ':' initialized_structure
	;

fb_name_decl 
	: fb_name_list ':' function_block_type_name (':=' structure_initialization )?
	;

fb_name_list 
	: fb_name (',' fb_name)*
		;

fb_name 
	: IDENTIFIER
	;

function_block_type_name 
	: standard_function_block_name 
	| derived_function_block_name
	;

// FIXME hier muessen alle fest definierten function blocks rein
//standard_function_block_name : <as defined in clause 2.5.2.3 of the standard>
//momentan ein blöder platzhalter für einen function
standard_function_block_name : 'FB_A';

derived_function_block_name 
	: IDENTIFIER
	;

output_declarations 
	: 'VAR_OUTPUT' ('RETAIN' | 'NON_RETAIN')? var_init_decl ';' (var_init_decl ';')* 'END_VAR'
	;

input_output_declarations 
	: 'VAR_IN_OUT' var_declaration ';' (var_declaration ';')* 'END_VAR'
	;

var_declaration 
	: temp_var_decl 
	| fb_name_decl
	;

temp_var_decl 
	: var1_declaration 
	| array_var_declaration 
	| structured_var_declaration 
	| string_var_declaration
	;

var1_declaration 
	: var1_list ':' (simple_specification | subrange_specification | enumerated_specification)
	;

array_var_declaration 
	: var1_list ':' array_specification
	;

structured_var_declaration 
	: var1_list ':' structure_type_name
	;

var_declarations 
	: 'VAR' ('CONSTANT')? var_init_decl ';' ((var_init_decl ';'))* 'END_VAR'
	;

retentive_var_declarations 
	: 'VAR' 'RETAIN' var_init_decl ';' (var_init_decl ';')* 'END_VAR'
	;

non_retentive_var_declarations 
	: 'VAR' var_init_decl ';' (var_init_decl ';')* 'END_VAR'
	;

located_var_declarations 
	: 'VAR' ('CONSTANT' | 'RETAIN' | 'NON_RETAIN')? located_var_decl ';' (located_var_decl ';')* 'END_VAR'
	;

located_var_decl 
	: (variable_name)? location ':' located_var_spec_init
	;

external_var_declarations 
	: 'VAR_EXTERNAL' ('CONSTANT')? external_declaration ';' (external_declaration ';')* 'END_VAR'
	;

external_declaration 
	: global_var_name ':' (simple_specification | subrange_specification | enumerated_specification | array_specification | structure_type_name | function_block_type_name)
	;

global_var_name 
	: IDENTIFIER
	;

global_var_declarations 
	: 'VAR_GLOBAL' ('CONSTANT' | 'RETAIN')? global_var_decl ';' (global_var_decl ';')* 'END_VAR'
	;

global_var_decl 
	: global_var_spec ':' ( located_var_spec_init | function_block_type_name )?
	;

global_var_spec 
	: global_var_list 
	| (global_var_name)? location
	;

located_var_spec_init 
	: simple_spec_init 
	| subrange_spec_init 
	| enumerated_spec_init 
	| array_spec_init 
	| initialized_structure 
	| single_byte_string_spec 
	| double_byte_string_spec
	;

location 
	: 'AT' direct_variable
	;

global_var_list 
	: global_var_name (',' global_var_name)*
	;

string_var_declaration 
	: single_byte_string_var_declaration 
	| double_byte_string_var_declaration
	;

single_byte_string_var_declaration 
	: var1_list ':' single_byte_string_spec
	;

single_byte_string_spec 
	: STRING ('[' INTEGER ']')? (':=' single_byte_character_string)?
	;

double_byte_string_var_declaration 
	: var1_list ':' double_byte_string_spec
	;

double_byte_string_spec 
	: WSTRING ('[' INTEGER ']')? (':=' double_byte_character_string)?
	;

incompl_located_var_declarations 
	: 'VAR' ('RETAIN'|'NON_RETAIN')? incompl_located_var_decl ';' (incompl_located_var_decl ';')* 'END_VAR'
	;

incompl_located_var_decl 
	: variable_name incompl_location ':' var_spec
	;

incompl_location 
	: 'AT' '%' (LOCATION_PREFIX) '*'
	;

var_spec 
	: simple_specification 
	| subrange_specification 
	| enumerated_specification 
	| array_specification 
	| structure_type_name 
	| STRING ('[' INTEGER ']')? 
	| WSTRING ('['INTEGER ']')?
	;

subrange_type_declaration 
	: subrange_type_name ':' subrange_spec_init
	;

subrange_spec_init 
	: subrange_specification(':=' SIGNED_INTEGER)?
	;

subrange_specification 
	: integer_type_name '(' subrange')' 
	| subrange_type_name
	;

enumerated_type_declaration 
	: enumerated_type_name ':' enumerated_spec_init
	;

enumerated_spec_init 
	: enumerated_specification (':=' enumerated_value)?
	;

enumerated_specification 
	: ('(' enumerated_value (',' enumerated_value)* ')') 
	| enumerated_type_name
	;

enumerated_value 
	: (enumerated_type_name '#')? IDENTIFIER
	;

array_type_declaration 
	: array_type_name ':' array_spec_init
	;

array_spec_init 
	: array_specification (':=' array_initialization)?
	;

array_specification 
	: array_type_name | 'ARRAY' '[' subrange (',' subrange)* ']' 'OF' non_generic_type_name
	;

array_initialization: '[' array_initial_elements (',' array_initial_elements)* ']'
	;

array_initial_elements 
	: array_initial_element | INTEGER '('(array_initial_element)? ')'
	;

array_initial_element 
	: constant 
	| enumerated_value 
	| structure_initialization 
	| array_initialization
	;

structure_type_declaration 
	: structure_type_name ':' structure_specification
	;

structure_specification 
	: structure_declaration 
	| initialized_structure
	;

initialized_structure 
	: structure_type_name (':=' structure_initialization)?
	;

structure_declaration 
	:'STRUCT' structure_element_declaration ';'(structure_element_declaration ';')*'END_STRUCT'
	;

structure_element_declaration 
	: structure_element_name ':'(simple_spec_init | subrange_spec_init | enumerated_spec_init | array_spec_init | initialized_structure)
	;

structure_element_name 
	: IDENTIFIER
	;

structure_initialization 
	: '(' structure_element_initialization (',' structure_element_initialization)* ')'
	;

structure_element_initialization 
	: structure_element_name ':=' (constant | enumerated_value | array_initialization | structure_initialization)
	;


string_type_declaration 
	: string_type_name ':' (STRING | WSTRING) ('[' INTEGER ']')? (':=' character_string)?
	;


function_var_decls 
	: 'VAR' ('CONSTANT')? var2_init_decl ';' (var2_init_decl ';')* 'END_VAR'
	;

var2_init_decl 
	: var1_init_decl 
	| array_var_init_decl 
	| structured_var_init_decl 
	| string_var_declaration
	;

function_name 
	: standard_function_name 
	| derived_function_name;

standard_function_name 
	: 'ABS'
	| 'SQRT'
	//t.b.d
	;

derived_function_name 
	: IDENTIFIER
	;
	
param_assignment 
	: ((variable_name ':=')? expression)
    | (('NOT')? variable_name '=>' variable);
                 
// --- variables

variable_name : IDENTIFIER;

variable 
	: direct_variable 
	| symbolic_variable;

symbolic_variable 
	: variable_name 
	| multi_element_variable  
	; 

multi_element_variable 
	: multi_element_variable LBRACK expression (',' expression)* RBRACK
	| multi_element_variable '.' field_selector
	| variable_name
	;

field_selector 
	: IDENTIFIER
	;

//direct_variable : '%' location_prefix size_prefix INTEGER (DOT INTEGER)*?;
direct_variable : '%' DIRECT_VAR_SPECIFIER;


// --- statements ---

statement_list : statement ';' (statement ';')*;

statement 
	: assignment_statement 
	| subprogram_control_statement
    | selection_statement 
    | iteration_statement
//  | NIL
	;
	
assignment_statement : variable ASSIGN expression;

subprogram_control_statement : fb_invocation | 'RETURN';

fb_invocation : fb_name LPAR (param_assignment (',' param_assignment)*)?RPAR;
	
selection_statement : if_statement | case_statement;

if_statement 
	: 'IF' expression 'THEN' statement_list
	('ELSIF' expression 'THEN' statement_list)*
	('ELSE' statement_list)?
	'END_IF';

case_statement 
	: 'CASE' expression 'OF'
	case_element
	('ELSE' statement_list)?
	'END_CASE';

case_element : case_list ':' statement_list;

case_list : case_list_element (',' case_list_element)*;

case_list_element 
	: subrange 
	| SIGNED_INTEGER 
//	| enumerated_value
	;

subrange 
	: SIGNED_INTEGER DOUBLEDOT SIGNED_INTEGER
	;

iteration_statement 
	: for_statement 
	| while_statement 
	| repeat_statement 
	| exit_statement;

for_statement : 'FOR' control_variable ':=' for_list 'DO' statement_list 'END_FOR';

control_variable : IDENTIFIER;

for_list : expression 'TO' expression ('BY' expression)?;

while_statement : 'WHILE' expression 'DO' statement_list 'END_WHILE';

repeat_statement : 'REPEAT' statement_list 'UNTIL' expression 'END_REPEAT';

exit_statement : 'EXIT';

//--------- Lexer Rules ---------



// --- TIME ---

DAYS 
	: FIXED_POINT ('d') 
	| INTEGER ('d') ('_')? HOURS
	;

HOURS 
	: FIXED_POINT ('h') 
	| INTEGER ('h')('_')? MINUTES
	;

MINUTES 
	: FIXED_POINT ('m') 
	| INTEGER ('m') ('_')? SECONDS
	;

SECONDS 
	: FIXED_POINT ('s') 
	| INTEGER ('s') ('_')?MILLISECONDS
	;

MILLISECONDS 
	: FIXED_POINT ('ms')
	| INTEGER ('ms')
	;
	

// --- Datatypes ---

SINT
	: 'SINT'
	;

INT
	: 'INT'
	;

DINT
	: 'DINT'
	;
	
LINT
	: 'LINT'
	;

USINT
	: 'USINT'
	;

UINT
	: 'UINT'
	;
	
UDINT
	: 'UDINT'
	;

ULINT
	: 'ULINT'
	;

REAL
	: 'REAL'
	;

LREAL
	: 'LREAL'
	;
	
TIME
	: ('TIME' | 'T')
	;
	
DATE
	: ('DATE' | 'D')
	;
	
DATETIME
	: ('DATE_AND_TIME' | 'DT')
	;
	
TIMEOFDAY
	: ('TIME_OF_DAY' | 'TOD')
	;

BOOL
	: 'BOOL'
	;	

BITSTRING
	: 'BYTE' 
	| 'WORD' 
	| 'DWORD' 
	| 'LWORD'
	;
	
GENERIC
	: 'ANY' 
	| 'ANY_DERIVED' 
	| 'ANY_ELEMENTARY' 
	| 'ANY_MAGNITUDE' 
	| 'ANY_NUM' 
	| 'ANY_REAL' 
	| 'ANY_INT' 
	| 'ANY_BIT' 
	| 'ANY_STRING' 
	| 'ANY_DATE';
	
STRING
	: 'STRING' 
	;
	
WSTRING
	:'WSTRING' 
//	| 'TIME' // poss. conflict with TIME decl.
	;
	

	
// --- Logic ---

OR  		
	: 'OR'
	;

XOR 		
	: 'XOR'
	;

AND 		
	: '&' 
	| 'AND'
	;

NOT 		
	: 'NOT'
	;

// --- comparison
EQU 		
	: '='
	;

NEQU		
	: '<>'
	;

LT  		
	: '<'
	;

GT  		
	: '>'
	;

LE  		
	: '<='
	;

GE  		
	: '>='
	;

// --- arithmetic
PLUS		
	: '+'
	;

MINUS	
	: '-'
	;

MUL		
	: '*'
	;

DIV		
	: '/'
	;

MOD		
	: 'MOD'
	;

POW		
	: '**'
	;

ASSIGN
	: ':='
	;
	
LPAR		
	: '('
	;

RPAR		
	: ')'
	;

LBRACK 
	: '['
	;
	
RBRACK 
	: ']'
	;

COLON
	: ':'
	;
	
DOT : '.'
	;
	
DOUBLEDOT
	: '..'
	;

BOOLEAN
	: BOOL '#' Bit
	| 'TRUE'
	| 'FALSE'
	;
	
FIXED_POINT 
	: (PLUS|MINUS)? INTEGER '.' INTEGER
	;

SIGNED_INTEGER 
	: (PLUS|MINUS) INTEGER
	;

EXPONENT 
	: ('E' | 'e')('+'|'-')? INTEGER
	;
	
INTEGER
	: Digit (('_')? Digit)*
	;

BINARY_INTEGER 
	: '2#' Bit (('_')? Bit)*
	;

OCTAL_INTEGER 
	: '8#' OctalDigit (('_')? OctalDigit)*
	;

HEX_INTEGER 
	: '16#' HexDigit(('_')? HexDigit)*
	;	

DIRECT_VAR_SPECIFIER
	: LOCATION_PREFIX SIZE_PREFIX? INTEGER (DOT INTEGER)*
	;

LOCATION_PREFIX
	: 'I'
	| 'Q'
	| 'M'
	; 

SIZE_PREFIX
	: 'X'
	| 'B'
	| 'W'
	| 'D'
	| 'L'
	;
	
IDENTIFIER
	: Letter LetterOrDigitOrUnderscore*
	;


	

HEX_BYTE
	: HexDigit HexDigit
	;
	
HEX_WORD
	: HexDigit HexDigit HexDigit HexDigit
	;

DB_STRING_LITERAL
	:	'"' DbStringCharacters? '"'
	;

SB_STRING_LITERAL
	:	'\'' SbStringCharacters? '\''
	;

fragment
DbStringCharacters
	:	DbStringCharacter+
	;

fragment
SbStringCharacters
	:	SbStringCharacter+
	;

fragment
DbStringCharacter
	:	~["$\r\n]
	|	EscapeSequence
	|   DbQuoteEscapeSequence
	;

fragment
SbStringCharacter
	:	~[$\r\n]
	|	EscapeSequence
	|	SbQuoteEscapeSequence
	;

fragment
EscapeSequence
	:	'$' [$LlNnPpRrTt]
	;

fragment
DbQuoteEscapeSequence
	:	'$"'
	;
	
fragment
SbQuoteEscapeSequence
	:	'$\''
	;

fragment LetterOrDigitOrUnderscore
    : Letter
    | Digit 
    | '_'
    ;

fragment Letter
    : [a-zA-Z]
    ;

fragment Digit 
	: 
	('0'..'9')
	;

fragment OctalDigit
	: ('0'..'7')
	;

fragment Bit
	: [0-1]
	;
	
fragment HexDigit
	: Digit 
	| [A-Fa-f]
	;

	    
// Whitespace and Comments
WS  :  [ \r\t\u000C\n]+ -> skip
    ;
    
COMMENT 
	:   ('(*' | '/*') .*? ('*)' | '*/') -> skip
  	;
// newer release introduced line comments    
LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;

