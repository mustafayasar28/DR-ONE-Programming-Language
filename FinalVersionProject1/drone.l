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
goto (goto)
lockPosition (lockPosition)
unlockPosition (unlockPosition)
giveName (giveName)
boolean_operator (AND)|(OR)|(NOT)
boolean (true|false)
str_letter [a-zA-Z]
digit   [0-9]
int {sign}?{digit}+
string (\"[^\"\n]*\")
identifier [\_]?({str_letter}|{digit})+
%%
{assignment_op} printf("ASSIGNMENT_OP ");
{plus} printf("PLUS ");
{minus} printf("MINUS ");
{multiplication} printf("MULTIPLICATION_OP ");
{division} printf("DIVISION_OP ");
{power} printf("POWER_OP ");
{equality_check} printf("EQUAL_CHECK "); 
{smaller_than} printf("SMALLER ") ;
{smaller_or_equals}  printf("SMALLER_OR_EQUAL "); 
{larger_than} printf("GREATER ");
{larger_or_equals} printf("GREATER_OR_EQUAL "); 
{not_equal} printf("NOT_EQUAL ");
{space}   ;
{newLine} printf("\n");
\;   printf("SEMI_COLON ");
\,   printf("COMMA ");
\(   printf("LP ");
\)   printf("RP ");
\{  printf("LBRACE ");
\}  printf("RBRACE ");
{increment}	     printf("INCREMENT ");
{decrement}	     printf("DECREMENT ");
{comment} printf("COMMENT_LINE ");
{if}    printf("IF ");
{else}  printf("ELSE ");
{var}   printf("DECL ");
{for}   printf("FOR ");
{while} printf("WHILE ");
{return} printf("FUNC_RETURN ");
{drin}  printf("DRIN ");
{drout} printf("DROUT ");
{start} printf("BEGIN_PROGRAM");
{stop} printf("END_PROGRAM");
{drfunction} printf("FUNCTION_DECL ");
{incline}   printf("INCLINE ");
{altitude}  printf("ALTITUDE ");
{temperature}   printf("TEMPERATURE ");
{acceleration}  printf("ACCELERATION ");
{speed}     printf("SPEED ");
{cameraOn}  printf("CAMERA_ON ");
{cameraOff} printf("CAMERA_OFF ");
{takePicture}   printf("TAKE_PICTURE ");
{readTime}  printf("READ_TIME ");
{connect}   printf("CONNECT ");
{setSpeed}  printf("SET_SPEED ");
{setAltitude} printf("SET_ALTITUDE ");
{setIncline}    printf("SET_INCLINE ");
{droneUp} printf("MOVE_DRONE_UP ");
{droneDown} printf("MOVE_DRONE_DOWN ");
{droneRight} printf("MOVE_DRONE_RIGHT "); 
{droneLeft} printf("MOVE_DRONE_LEFT ");
{droneForward} printf("MOVE_DRONE_FORWARD ");
{droneBackward} printf("MOVE_DRONE_BACKWARD ");
{rotateClockwise} printf("ROTATE_CW ");
{rotateCounterClockwise} printf("ROTATE_CCW ");
{goto} printf("GO_TO_POSITION ");
{lockPosition} printf("LOCK_POSITION ");
{unlockPosition} printf("UNLOCK_POSITION ");
{giveName} printf("GIVE_NAME ");
{boolean}   printf("BOOLEAN ");
{boolean_operator}  printf("BOOLEAN_OP ");
{int}   printf("INTEGER ");
{string}   printf("STRING ");
{identifier} printf("IDENTIFIER ");

%% 

int yywrap() {}

int main() {

extern FILE *yyin;
yyin = fopen("input.txt", "r");
yylex();

printf("******************* END OF INPUT FILE 1 ********************\n");

yyin = fopen("input2.txt", "r");
yylex();

printf("******************* END OF INPUT FILE 2 ********************\n");

yyin = fopen("input3.txt", "r");
yylex();

return 0;
}
