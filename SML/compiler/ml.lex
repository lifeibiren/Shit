
datatype lexresult = EOF | ID of string | EQ | IMPORT | INTEGER of int | String of string | VAL | DASH

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

fun printYytext text = print (text ^ "\n")
 
%% 
%full

%s TEXT TEXT_FMT  BLOCK_COMMENT LINE_COMMENT  LINE_DIR1 LINE_DIR2 LINE_DIR3 LINE_DIR4;

%structure MlLex

ws=\t|"\011"|"\012"|" ";
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

alphanum=[A-Za-z0-9'_];
alphanumId=[A-Za-z]{alphanum}*;
sym="!"|"%"|"&"|"$"|"#"|"+"|"-"|"/"|":"|"<"|"="|">"|"?"|"@"|"\\"|"~"|"`"|"^"|"|"|"*";
symId={sym}+;

tyvarId="'"{alphanum}*;
longSymId=({alphanumId}".")+{symId};
longAlphanumId=({alphanumId}".")+{alphanumId};

decDigit=[0-9];
decnum={decDigit}("_"*{decDigit})*;
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}("_"*{hexDigit})*;
binDigit=[0-1];
binnum={binDigit}("_"*{binDigit})*;
frac="."{decnum};
exp=[eE](~?){decnum};
real=(~?)(({decnum}{frac}?{exp})|({decnum}{frac}{exp}?));

nZDecDigit= [1-9];
integer= {nZDecDigit}{decDigit}*;
string=\"[^\"]+\";

%%
\n => (!linenum = !linenum + 1; lex());
{ws} => (lex());
{integer}  => (INTEGER (case (Int.fromString yytext) of
                    SOME i => i
                 |  NONE   => 0));
{alphanumId} => (ID yytext);
"val" => (VAL);
"=" => (EQ);
"_import" => (IMPORT);
"_" => (DASH);
{string} => (String yytext);
. => (error ("ignoring bad character " ^ yytext); lex());
