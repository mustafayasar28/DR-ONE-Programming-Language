%{
#include <stdio.h>
void yyerror(char *);
%}

assignment_op \=
plus \+
minus \-
multiplication \*
division \/
power \^
space   [ \t]
newLine [\n]
equality_check (==)
smaller_than	 \<
smaller_or_equals (<=)
larger_than \>
larger_or_equals (>=)
not_equal (!=)
comment (\*CM\*)[^\n]*(\*CM\*)
sign    [-+]
increment (\+\+)
decrement (\-\-)
start (start)
stop (stop)
if  (if)
else (else)
var (var)
for (for)
while (while)
return (return)
drin (drin)
drout (drout)
drfunction (drfunction)
fdrin (fdrin)
fdrout (fdrout)
incline (incline)
altitude (altitude)
temperature (temperature)
acceleration (acceleration)
speed   (speed)
cameraOn    (cameraOn)
cameraOff   (cameraOff)
takePicture (takePicture)
readTime    (readTime)
connect (connect)
setSpeed    (setSpeed)
setAltitude (setAltitude)
setIncline  (setIncline)
droneUp (droneUp)
droneDown(droneDown)
droneRight (droneRight)
droneLeft (droneLeft)
droneForward (droneForward)
droneBackward (droneBackward)
rotateClockwise (rotateCW)
rotateCounterClockwise (rotateCCW)
gotoPos (gotoPos)
lockPosition (lockPosition)
unlockPosition (unlockPosition)
giveName (giveName)
logical_and (AND)
logical_or (OR)
logical_not (NOT)
boolean (true|false)
str_letter [a-zA-Z]
digit   [0-9]
int {sign}?{digit}+
string (\"[^\"\n]*\")
identifier [\_]?({str_letter}|{digit})+

%option yylineno
%%
{assignment_op} return ASSIGNMENT_OP;
{plus} return PLUS;
{minus} return MINUS;
{multiplication} return MULTIPLICATION_OP;
{division} return DIVISION_OP;
{power} return POWER_OP;
{equality_check} return EQUAL_CHECK; 
{smaller_than} return SMALLER;
{smaller_or_equals}  return SMALLER_OR_EQUAL; 
{larger_than} return GREATER;
{larger_or_equals} return GREATER_OR_EQUAL; 
{not_equal} return NOT_EQUAL;
{space}   ; 
{newLine} ;
\;   return SEMI_COLON;
\,   return COMMA;
\(   return LP;
\)   return RP;
\{  return LBRACE;
\}  return RBRACE;
{increment}	     return INCREMENT;
{decrement}	     return DECREMENT;
{comment} return COMMENT_LINE;
{if}    return IF;
{else}  return ELSE;
{var}   return DECLARATION;
{for}   return FOR;
{while} return WHILE;
{return} return FUNC_RETURN;
{drin}  return DRIN;
{drout} return DROUT;
{fdrin} return FILE_DRIN ;
{fdrout} return FILE_DROUT;
{start} return BEGIN_PROGRAM;
{stop} return END_PROGRAM;
{drfunction} return FUNCTION_DECL;
{incline}   return INCLINE;
{altitude}  return ALTITUDE;
{temperature}   return TEMPERATURE;
{acceleration}  return ACCELERATION;
{speed}     return SPEED;
{cameraOn}  return CAMERA_ON;
{cameraOff} return CAMERA_OFF;
{takePicture}   return TAKE_PICTURE;
{readTime}  return READ_TIME;
{connect}   return CONNECT;
{setSpeed}  return SET_SPEED;
{setAltitude} return SET_ALTITUDE;
{setIncline}    return SET_INCLINE;
{droneUp} return MOVE_DRONE_UP;
{droneDown} return MOVE_DRONE_DOWN;
{droneRight} return MOVE_DRONE_RIGHT; 
{droneLeft} return MOVE_DRONE_LEFT;
{droneForward} return MOVE_DRONE_FORWARD;
{droneBackward} return MOVE_DRONE_BACKWARD;
{rotateClockwise} return ROTATE_CW;
{rotateCounterClockwise} return ROTATE_CCW;
{gotoPos} return GO_TO_POSITION;
{lockPosition} return LOCK_POSITION;
{unlockPosition} return UNLOCK_POSITION;
{giveName} return GIVE_NAME;
{boolean}   return BOOLEAN;
{logical_and} return LOGICAL_AND;
{logical_or} return LOGICAL_OR;
{logical_not} return LOGICAL_NOT;
{int}   return INTEGER;
{string}   return STRING;
{identifier} return IDENTIFIER;
%% 

int yywrap(void){
	return 1;
}