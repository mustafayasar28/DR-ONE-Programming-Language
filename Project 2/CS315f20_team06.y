%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
void yyerror(char* s);
extern int yylineno;
%}

%token ASSIGNMENT_OP
%token PLUS
%token MINUS
%token MULTIPLICATION_OP
%token DIVISION_OP
%token POWER_OP
%token EQUAL_CHECK
%token SMALLER
%token SMALLER_OR_EQUAL
%token GREATER
%token GREATER_OR_EQUAL
%token NOT_EQUAL
%token SEMI_COLON
%token COMMA
%token LP
%token RP
%token LBRACE
%token RBRACE
%token INCREMENT
%token DECREMENT
%token COMMENT_LINE
%token IF
%token ELSE
%token DECLARATION
%token FOR
%token WHILE
%token FUNC_RETURN
%token DRIN 
%token DROUT
%token FILE_DRIN 
%token FILE_DROUT
%token BEGIN_PROGRAM
%token END_PROGRAM
%token FUNCTION_DECL
%token INCLINE
%token ALTITUDE
%token TEMPERATURE
%token ACCELERATION
%token SPEED
%token CAMERA_ON
%token CAMERA_OFF
%token TAKE_PICTURE
%token READ_TIME
%token CONNECT
%token SET_SPEED
%token SET_ALTITUDE
%token SET_INCLINE
%token MOVE_DRONE_UP
%token MOVE_DRONE_DOWN
%token MOVE_DRONE_RIGHT
%token MOVE_DRONE_LEFT
%token MOVE_DRONE_FORWARD
%token MOVE_DRONE_BACKWARD
%token ROTATE_CW
%token ROTATE_CCW
%token GO_TO_POSITION
%token LOCK_POSITION
%token UNLOCK_POSITION
%token GIVE_NAME
%token BOOLEAN
%token LOGICAL_AND
%token LOGICAL_OR
%token LOGICAL_NOT
%token INTEGER
%token STRING
%token IDENTIFIER

%start program
%right ASSIGNMENT_OP
%left PLUS MINUS 
%left MULTIPLICATION_OP DIVISION_OP POWER_OP

%% 

//Program
program: BEGIN_PROGRAM statements END_PROGRAM
;

//Statement Definitions
statements: statement SEMI_COLON 
      | statements statement SEMI_COLON
                  | statements COMMENT_LINE 
                  | COMMENT_LINE
	;
 statement : assignment_stmt
      | post_incr
      | post_decr
      | func_call
      | declaration
      | conditional_stmt
      | while_stmt | for_stmt | function_no_return      
      | function_return | input | output
	      | file_input | file_output 
	;

block_statement :  LBRACE  inside_statements RBRACE
                |   LBRACE  RBRACE
;

inside_statements : inside_statement SEMI_COLON 
                  | inside_statements inside_statement SEMI_COLON
;

inside_statement : assignment_stmt | post_incr | post_decr
     | func_call | declaration | conditional_stmt
 		     | while_stmt | for_stmt | input | output |  file_input | file_output 
	;

//Assignment Statement
assignment_stmt : IDENTIFIER ASSIGNMENT_OP assignment_expr
	;

assignment_expr : expr | STRING | BOOLEAN
     | input | file_input 
	;

//Post Statements
post_incr : IDENTIFIER INCREMENT
	;

post_decr : IDENTIFIER DECREMENT
	;

//Variable declaration statement
declaration : DECLARATION IDENTIFIER 
| DECLARATION assignment_stmt
;

constant : INTEGER | STRING | BOOLEAN 
	;

//Function call statement
func_call : IDENTIFIER LP  RP 
	          | IDENTIFIER  LP  defined_parameters  RP
                      | primitive_func_call
	;


//Loop Statements
for_stmt: FOR LP for_initial SEMI_COLON expr SEMI_COLON for_update  RP block_statement
 | FOR LP SEMI_COLON expr SEMI_COLON for_update RP   block_statement
;

for_initial: for_initial_helper | for_initial_helper COMMA for_initial
;

for_initial_helper : DECLARATION IDENTIFIER ASSIGNMENT_OP arithm_term 
     | IDENTIFIER ASSIGNMENT_OP arithm_term
;

for_update: arithm_term | post_incr | post_decr
	;

while_stmt: WHILE LP expr RP block_statement
	;

// Conditional Statements
conditional_stmt : IF LP expr RP block_statement
                | IF LP expr RP block_statement ELSE block_statement
 ;


//Input and Output Statements
input: DRIN LP RP
;

output: 
	 DROUT LP assignment_expr RP
	;

file_input : FILE_DRIN LP STRING RP 
                 | FILE_DRIN LP IDENTIFIER RP
	;

file_output : FILE_DROUT LP assignment_expr COMMA IDENTIFIER RP
      | FILE_DROUT LP assignment_expr COMMA STRING RP
;

//Function Declarations
function_no_return: FUNCTION_DECL IDENTIFIER LP parameters RP block_statement
	        | FUNCTION_DECL IDENTIFIER LP  RP block_statement 
;

function_return: 
FUNCTION_DECL IDENTIFIER LP parameters RP  LBRACE  inside_statements FUNC_RETURN constant SEMI_COLON RBRACE
| FUNCTION_DECL IDENTIFIER LP parameters RP  LBRACE  FUNC_RETURN constant SEMI_COLON RBRACE
 | FUNCTION_DECL IDENTIFIER LP parameters  RP  LBRACE  
inside_statements FUNC_RETURN IDENTIFIER SEMI_COLON RBRACE
| FUNCTION_DECL IDENTIFIER LP  RP  LBRACE  
inside_statements   FUNC_RETURN constant SEMI_COLON RBRACE
| FUNCTION_DECL IDENTIFIER LP RP  LBRACE  
FUNC_RETURN constant SEMI_COLON RBRACE
| FUNCTION_DECL IDENTIFIER LP  RP  LBRACE  
inside_statements FUNC_RETURN IDENTIFIER SEMI_COLON RBRACE
| FUNCTION_DECL IDENTIFIER LP parameters  RP  LBRACE  
FUNC_RETURN IDENTIFIER SEMI_COLON RBRACE
;

parameters : DECLARATION IDENTIFIER
        | INTEGER
        | STRING
        | INTEGER COMMA parameters
        | STRING COMMA parameters
        | DECLARATION IDENTIFIER COMMA parameters
;

defined_parameters : IDENTIFIER 
         | INTEGER 
         | STRING
         | STRING COMMA defined_parameters
         | INTEGER COMMA defined_parameters
         | IDENTIFIER  COMMA defined_parameters
	;

//Expressions
expr: logical_term 
       | expr LOGICAL_OR logical_term
;

logical_term: logical_prec 
                   | logical_term LOGICAL_AND logical_prec 
	;

logical_prec: LOGICAL_NOT logical_prec 
                    | relational_expr
        | relational_expr LOGICAL_NOT logical_prec
;

relational_expr : arithm_term relational_opr arithm_term | arithm_term
	;

arithm_term:  term 
        | arithm_term PLUS term 
        | arithm_term MINUS term
	;
term : 	power 
| term MULTIPLICATION_OP power
| term DIVISION_OP power
;

power : factor POWER_OP power | factor
	;

factor :  LP expr  RP | INTEGER | IDENTIFIER | func_call
;

relational_opr:  SMALLER | SMALLER_OR_EQUAL | GREATER | GREATER_OR_EQUAL 
                      | EQUAL_CHECK | NOT_EQUAL
	;
			
//Primitive functions
primitive_func_call: read_incl 
        | read_altitude | read_temperature | read_acceleration  
        | read_speed | camera_on | camera_off 
        | take_picture | read_time | connect_wifi 
        | set_speed | set_altitude | set_incl | drone_up 
        | drone_down | drone_right | drone_left 
        | drone_forward | drone_backward 
        | rotate_clockwise | rotate_counterclockwise | gotoPos 
        | lock_position | unlock_position | give_name
;

read_incl : INCLINE LP  RP 
;

read_altitude : ALTITUDE LP  RP
	;
read_temperature : TEMPERATURE LP  RP
	;
read_acceleration : ACCELERATION LP  RP
	;
read_speed : SPEED LP  RP
	;
camera_on : CAMERA_ON LP  RP
	;
camera_off : CAMERA_OFF LP  RP
                   ;
take_picture : TAKE_PICTURE LP  RP
                     ;
read_time : READ_TIME LP  RP
                 ;
connect_wifi : CONNECT LP  STRING  COMMA STRING  RP
                      ;
set_speed : SET_SPEED LP  INTEGER  RP
                 ;
set_altitude : SET_ALTITUDE LP  INTEGER  RP
                    ;
set_incl : SET_INCLINE LP  INTEGER  RP
              ;
drone_up : MOVE_DRONE_UP LP  INTEGER  RP
                ;
drone_down : MOVE_DRONE_DOWN LP  INTEGER  RP
                     ;
drone_right : MOVE_DRONE_RIGHT LP  INTEGER  RP
                    ;
drone_left : MOVE_DRONE_LEFT LP  INTEGER  RP
                 ;
drone_forward : MOVE_DRONE_FORWARD LP  INTEGER  RP
                        ;
drone_backward : MOVE_DRONE_BACKWARD LP  INTEGER  RP
                            ;
rotate_clockwise : ROTATE_CW LP  INTEGER  RP
                            ;
rotate_counterclockwise : ROTATE_CCW LP  INTEGER  RP
                                       ;
gotoPos : GO_TO_POSITION LP INTEGER COMMA INTEGER COMMA INTEGER COMMA INTEGER RP 
              ;
lock_position : LOCK_POSITION LP RP
                       ;
unlock_position : UNLOCK_POSITION LP RP
                          ;
give_name : GIVE_NAME LP STRING RP
                   ;
%%
#include "lex.yy.c"
void yyerror(char *s){
	fprintf(stdout, "line %d: %s\n", yylineno, s);
}

int main(void){
yyparse();
if( yynerrs < 1 ){
		printf("Parsing is successful\n");
}
return 0;
}
