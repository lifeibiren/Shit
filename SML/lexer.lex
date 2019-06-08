
datatype CSymbol = 
      AUTO
    | BREAK
    | CASE
    | CHAR
    | CONST
    | CONTINUE
    | DEFAULT
    | DO
    | DOUBLE
    | ELSE
    | ENUM
    | EXTERN
    | FLOAT
    | FOR
    | GOTO
    | IF
    | INLINE
    | INT
    | LONG
    | REGISTER
    | RESTRICT
    | SHORT
    | SIGNED
    | SIZEOF
    | STATIC
    | SWITCH
    | TYPEDEF
    | UNION
    | UNSIGNED
    | VOID
    | VOLATILE
    | WHILE
    | ATOMIC
    | BOOL
    | COMPLEX
    | GENERIC
    | IMAGINARY
    | NORETURN
    | STATIC_ASSERT
    | THREAD_LOCAL
    | FUNC_NAME
    | I_CONSTANT
    | F_CONSTANT
    | STRING_LITERAL
    | ELLIPSIS
    | RIGHT_ASSIGN
    | LEFT_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
    | RIGHT_OP
    | LEFT_OP
    | INC_OP
    | DEC_OP
    | PTR_OP
    | AND_OP
    | OR_OP
    | NOT_OP
    | LE_OP
    | GE_OP
    | EQ_OP
    | NE_OP
    | SEMICOLON
    | L_BRACE
    | R_BRACE
    | COMMA
    | COLON
    | ASSIGN
    | L_PARENTHESIS
    | R_PARENTHESIS
    | L_BRACKETS
    | R_BRACKETS
    | MEMBER_OP
    | BAND_REF_OP
    | BXOR_OP
    | BOR_OP
    | BNOT_OP
    | DIV_OP
    | MOD_OP
    | MUL_DEREF_OP
    | PLUS_OP
    | MINUS_OP
    | LT_OP
    | GT_OP
    | Q_OP
    
datatype symbol = Symbol of CSymbol
datatype position = Position of int * int (* row, column *)
datatype token = Token of symbol * position * string
datatype lexresult = EOF | token

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

fun printYytext text = print (text ^ "\n")
fun makeToken sym yytext pos = Token (Symbol sym, yytext, Position (!linenum, !pos))
fun comment 
%%
%structure FilterLex
O   [0-7];
D   [0-9];
NZ  [1-9];
L   [a-zA-Z_];
A   [a-zA-Z_0-9];
H   [a-fA-F0-9];
HP  (0[xX]);
E   ([Ee][+-]?{D}+);
P   ([Pp][+-]?{D}+);
FS  (f|F|l|L);
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?));
CP  (u|U|L);
SP  (u8|u|U|L);
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+));
WS  [ \t\v\n\f];

%%
\n => (!linenum = !linenum + 1; lex());


"/*"                                    { comment(); }
"//".*                                    { /* consume //-comment */ }

"auto"					{ makeToken AUTO yytext yypos }
"break"					{ return(BREAK); }
"case"					{ return(CASE); }
"char"					{ return(CHAR); }
"const"					{ return(CONST); }
"continue"				{ return(CONTINUE); }
"default"				{ return(DEFAULT); }
"do"					{ return(DO); }
"double"				{ return(DOUBLE); }
"else"					{ return(ELSE); }
"enum"					{ return(ENUM); }
"extern"				{ return(EXTERN); }
"float"					{ return(FLOAT); }
"for"					{ return(FOR); }
"goto"					{ return(GOTO); }
"if"					{ return(IF); }
"inline"				{ return(INLINE); }
"int"					{ return(INT); }
"long"					{ return(LONG); }
"register"				{ return(REGISTER); }
"restrict"				{ return(RESTRICT); }
"return"				{ return(RETURN); }
"short"					{ return(SHORT); }
"signed"				{ return(SIGNED); }
"sizeof"				{ return(SIZEOF); }
"static"				{ return(STATIC); }
"struct"				{ return(STRUCT); }
"switch"				{ return(SWITCH); }
"typedef"				{ return(TYPEDEF); }
"union"					{ return(UNION); }
"unsigned"				{ return(UNSIGNED); }
"void"					{ return(VOID); }
"volatile"				{ return(VOLATILE); }
"while"					{ return(WHILE); }
"_Alignas"                              { return ALIGNAS; }
"_Alignof"                              { return ALIGNOF; }
"_Atomic"                               { return ATOMIC; }
"_Bool"                                 { return BOOL; }
"_Complex"                              { return COMPLEX; }
"_Generic"                              { return GENERIC; }
"_Imaginary"                            { return IMAGINARY; }
"_Noreturn"                             { return NORETURN; }
"_Static_assert"                        { return STATIC_ASSERT; }
"_Thread_local"                         { return THREAD_LOCAL; }
"__func__"                              { return FUNC_NAME; }

{L}{A}*					{ return check_type(); }

{HP}{H}+{IS}?				{ return I_CONSTANT; }
{NZ}{D}*{IS}?				{ return I_CONSTANT; }
"0"{O}*{IS}?				{ return I_CONSTANT; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return I_CONSTANT; }

{D}+{E}{FS}?				{ return F_CONSTANT; }
{D}*"."{D}+{E}?{FS}?			{ return F_CONSTANT; }
{D}+"."{E}?{FS}?			{ return F_CONSTANT; }
{HP}{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}*"."{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}+"."{P}{FS}?			{ return F_CONSTANT; }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return STRING_LITERAL; }

"..."					{ return ELLIPSIS; }
">>="					{ return RIGHT_ASSIGN; }
"<<="					{ return LEFT_ASSIGN; }
"+="					{ return ADD_ASSIGN; }
"-="					{ return SUB_ASSIGN; }
"*="					{ return MUL_ASSIGN; }
"/="					{ return DIV_ASSIGN; }
"%="					{ return MOD_ASSIGN; }
"&="					{ return AND_ASSIGN; }
"^="					{ return XOR_ASSIGN; }
"|="					{ return OR_ASSIGN; }
">>"					{ return RIGHT_OP; }
"<<"					{ return LEFT_OP; }
"++"					{ return INC_OP; }
"--"					{ return DEC_OP; }
"->"					{ return PTR_OP; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!="					{ return NE_OP; }
";"					{ return ';'; }
("{"|"<%")				{ return '{'; }
("}"|"%>")				{ return '}'; }
","					{ return ','; }
":"					{ return ':'; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
("["|"<:")				{ return '['; }
("]"|":>")				{ return ']'; }
"."					{ return '.'; }
"&"					{ return '&'; }
"!"					{ return '!'; }
"~"					{ return '~'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"%"					{ return '%'; }
"<"					{ return '<'; }
">"					{ return '>'; }
"^"					{ return '^'; }
"|"					{ return '|'; }
"?"					{ return '?'; }

{WS}+					{ lex()/* whitespace separates tokens */ }

.     => (error ("ignoring bad character " ^ yytext); lex());
