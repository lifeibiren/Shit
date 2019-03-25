
datatype csymbol = 
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
    | RETURN
    | SHORT
    | SIGNED
    | SIZEOF
    | STATIC
    | STRUCT
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
    | L_BRACKET
    | R_BRACKET
    | MEMBER_OP
    | BAND_REF_OP
    | BXOR_OP
    | BOR_OP
    | BNOT_OP
    | DIV_OP
    | MOD_OP
    | START_OP
    | PLUS_OP
    | MINUS_OP
    | LESS_OP
    | GREATER_OP
    | QUES_OP
    | IDENTIFIER
    
datatype symbol = Symbol of csymbol
datatype position = Position of int * int (* row, column *)
datatype token = Token of symbol * string * position | EOF
type lexresult = token

val linenum = ref 1
val column = ref 1
val lineStart = ref 1
val columnStart = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

fun printYytext text = print (text ^ "\n")

fun count yytext = 
    let
        (* save starting position of current symbol *)
        val _ = lineStart := !linenum
        val _ = columnStart := !column
        fun loop yytext off = 
            if off < String.size yytext 
            then case String.sub (yytext, off) of
                  #"\n" => (column := 1; linenum := !linenum + 1; loop yytext (off + 1))
                 | _   => (column := !column + 1; loop yytext (off + 1))
            else ()
    in
        loop yytext 0
    end

fun makeToken sym text = 
    let
        val _ = count text
    in
        Token (Symbol sym, text, Position (!lineStart, !columnStart))
    end

%%
%structure FilterLex

O  = [0-7];
D  = [0-9];
NZ = [1-9];
L  = [a-zA-Z_];
A  = [a-zA-Z_0-9];
H  = [a-fA-F0-9];
HP = (0[xX]);
E  = ([Ee][+-]?{D}+);
P  = ([Pp][+-]?{D}+);
FS = (f|F|l|L);
IS = (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?));
CP = (u|U|L);
SP = (u8|u|U|L);
ES = (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+));
WS = [\ \t\v\n\f];

%%
\n              => ( count yytext; lex());

"auto"          => ( makeToken AUTO yytext );
"break"         => ( makeToken BREAK yytext );
"case"          => ( makeToken CASE yytext );
"char"          => ( makeToken CHAR yytext );
"const"         => ( makeToken CONST yytext );
"continue"      => ( makeToken CONTINUE yytext );
"default"       => ( makeToken DEFAULT yytext );
"do"            => ( makeToken DO yytext );
"double"        => ( makeToken DOUBLE yytext );
"else"          => ( makeToken ELSE yytext );
"enum"          => ( makeToken ENUM yytext );
"extern"        => ( makeToken EXTERN yytext );
"float"         => ( makeToken FLOAT yytext );
"for"           => ( makeToken FOR yytext );
"goto"          => ( makeToken GOTO yytext );
"if"            => ( makeToken IF yytext );
"inline"        => ( makeToken INLINE yytext );
"int"           => ( makeToken INT yytext );
"long"          => ( makeToken LONG yytext );
"register"      => ( makeToken REGISTER yytext );
"restrict"      => ( makeToken RESTRICT yytext );
"makeToken"     => ( makeToken RETURN yytext );
"short"         => ( makeToken SHORT yytext );
"signed"        => ( makeToken SIGNED yytext );
"sizeof"        => ( makeToken SIZEOF yytext );
"static"        => ( makeToken STATIC yytext );
"struct"        => ( makeToken STRUCT yytext );
"switch"        => ( makeToken SWITCH yytext );
"typedef"       => ( makeToken TYPEDEF yytext );
"union"         => ( makeToken UNION yytext );
"unsigned"      => ( makeToken UNSIGNED yytext );
"void"          => ( makeToken VOID yytext );
"volatile"      => ( makeToken VOLATILE yytext );
"while"         => ( makeToken WHILE yytext );

{L}{A}*	        => ( makeToken IDENTIFIER yytext );

{HP}{H}+{IS}?   => ( makeToken I_CONSTANT yytext );
{NZ}{D}*{IS}?   => ( makeToken I_CONSTANT yytext );
"0"{O}*{IS}?    =>  ( makeToken I_CONSTANT yytext );
{CP}?"'"([^'\\\n]|{ES})+"'"	=> ( makeToken I_CONSTANT yytext );

{D}+{E}{FS}?    => ( makeToken F_CONSTANT yytext );
{D}*"."{D}+{E}?{FS}?  => ( makeToken F_CONSTANT yytext );
{D}+"."{E}?{FS}?  => ( makeToken F_CONSTANT yytext );
{HP}{H}+{P}{FS}?  => ( makeToken F_CONSTANT yytext );
{HP}{H}*"."{H}+{P}{FS}?  => ( makeToken F_CONSTANT yytext );
{HP}{H}+"."{P}{FS}?  => ( makeToken F_CONSTANT yytext );

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+ => ( makeToken STRING_LITERAL yytext );


"..."           => ( makeToken ELLIPSIS yytext );
">>="           => ( makeToken RIGHT_ASSIGN yytext ); 
"<<="           => ( makeToken LEFT_ASSIGN yytext );	
"+="            => ( makeToken ADD_ASSIGN yytext );	
"-="            => ( makeToken SUB_ASSIGN yytext );	
"*="            => ( makeToken MUL_ASSIGN yytext );	
"/="            => ( makeToken DIV_ASSIGN yytext );	
"%="            => ( makeToken MOD_ASSIGN yytext );	
"&="            => ( makeToken AND_ASSIGN yytext );	
"^="            => ( makeToken XOR_ASSIGN yytext );	
"|="            => ( makeToken OR_ASSIGN yytext );	
">>"            => ( makeToken RIGHT_OP yytext );	
"<<"            => ( makeToken LEFT_OP yytext );	
"++"            => ( makeToken INC_OP yytext );	
"--"            => ( makeToken DEC_OP yytext );	
"->"            => ( makeToken PTR_OP yytext );	
"&&"            => ( makeToken AND_OP yytext );	
"||"            => ( makeToken OR_OP yytext );	
"<="            => ( makeToken LE_OP yytext );	
">="            => ( makeToken GE_OP yytext );	
"=="            => ( makeToken EQ_OP yytext );	
"!="            => ( makeToken NE_OP yytext );	
";"             => ( makeToken SEMICOLON yytext );	
("{"|"<%")      => ( makeToken L_BRACE yytext );
("}"|"%>")      => ( makeToken R_BRACE yytext );
","             => ( makeToken COMMA yytext );	
":"             => ( makeToken SEMICOLON yytext );	
"="             => ( makeToken ASSIGN yytext );	
"("             => ( makeToken L_PARENTHESIS yytext );	
")"             => ( makeToken R_PARENTHESIS yytext );	
("["|"<:")      => ( makeToken L_BRACKET yytext );
("]"|":>")      => ( makeToken R_BRACKET yytext );
"."             => ( makeToken MEMBER_OP yytext );	
"&"             => ( makeToken BAND_REF_OP yytext );	
"!"             => ( makeToken NOT_OP yytext );	
"~"             => ( makeToken BNOT_OP yytext );	
"-"             => ( makeToken MINUS_OP yytext );	
"+"             => ( makeToken PLUS_OP yytext );	
"*"             => ( makeToken START_OP yytext );	
"/"             => ( makeToken DIV_OP yytext );	
"%"             => ( makeToken MOD_OP yytext );	
"<"             => ( makeToken LESS_OP yytext );	
">"             => ( makeToken GREATER_OP yytext );	
"^"             => ( makeToken BXOR_OP yytext );	
"|"             => ( makeToken BOR_OP yytext );	
"?"             => ( makeToken QUES_OP yytext );

{WS}+           => ( count yytext; lex()  ); 

.               => ( count yytext; error ("ignoring bad character " ^ yytext); lex());
