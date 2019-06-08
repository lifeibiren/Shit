
datatype Operator = LBRACKET | RBRACKET | L_SQUARE_BRACKET | R_SQUARE_BRACKET | 
                        MEMBER_PTR | MEMBER | AND | OR | NOT | BIT_AND | BIT_OR | BIT_NOT |
                        ABOVE | LESS | EQUAL | ABOVE_OR_EQUAL | LESS_OR_EQUAL | NOT_EQUAL | 
                        SLASH | ADD | SUB | MUL | DIV | MOD | XOR | SHL | SHR | INC | DEC |
                        ADDRESS_OF | CONDITION_Q | CONDICTION_SEMI | COMMA | SIZEOF
                     
datatype lexresult = EOF | LBRACE | RBRACE | 
                     LBRACKET | RBRACKET | INTEGER of int | ID of string

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

fun printYytext text = print (text ^ "\n")

%%
%structure FilterLex
ws            = [\ \t\n]+;
l_brace       = "{";
r_brace       = "}";
l_bracket     = "(";
r_bracket     = ")";
and           = "&&";
or            = "||";
not           = "!";

bit_and       = "&";
bit_or        = "|";
bit_not       = "~";
above         = ">";
less          = "<";
equal         = "=";
above_or_equal = ">=";
less_or_equal = "<=";
not_equal     = "!=";
slash         = "/";
add           = "+";
sub           = "-";
mul           = "*";
mod           = "%";
xor           = "^";
lsh           = "<<";
rsh           = ">>";

alpha         = [A-Za-z];
alpha_num     = [A-Za-z0-9];
dig           = [0-9];
nzdig         = [1-9];
dot           =  ".";
nonzerohex    = [1-9a-fA-F];
hexdigit      = [0-9a-fA-F];
semi          = ":";

ID            = (_|{alpha})(_|{alpha_num})*;
integer       = {nzdig}{dig}*;

%%
\n => (!linenum = !linenum + 1; lex());
{ws} => (lex());
{integer}  => ((printYytext yytext);
                INTEGER (case (Int.fromString yytext) of
                    SOME i => i
                 |  NONE   => 0));

{and}  =>   (printYytext yytext; AND);
{or}   => (printYytext yytext; OR);
{not}  => (printYytext yytext; NOT);
{ID}   => (printYytext yytext; ID yytext);
{l_brace} => (printYytext yytext; LBRACE);
{r_brace} => (printYytext yytext; RBRACE);

// operators
{l_bracket} => (printYytext yytext; LBRACKET);
{r_bracket}  => (printYytext yytext; RBRACKET);

{and}            => (printYytext yytext; ID yytext);
{or}             => (printYytext yytext; ID yytext);
{not}            => (printYytext yytext; ID yytext);

{bit_and}        => (printYytext yytext; ID yytext);
{bit_or}         => (printYytext yytext; ID yytext);
{bit_not}        => (printYytext yytext; ID yytext);
{above}          => (printYytext yytext; ID yytext);
{less}           => (printYytext yytext; ID yytext);
{equal}          => (printYytext yytext; ID yytext);
{above_or_equal} => (printYytext yytext; ID yytext);
{less_or_equal}  => (printYytext yytext; ID yytext);
{not_equal}     => (printYytext yytext; ID yytext);
{slash}         => (printYytext yytext; ID yytext);
{add}           => (printYytext yytext; ID yytext);
{sub}           => (printYytext yytext; ID yytext);
{mul}           => (printYytext yytext; ID yytext);
{mod}           => (printYytext yytext; ID yytext);
{xor}          => (printYytext yytext; ID yytext);
{lsh}           => (printYytext yytext; ID yytext);
{rsh}          => (printYytext yytext; ID yytext);

.     => (error ("ignoring bad character " ^ yytext); lex());
