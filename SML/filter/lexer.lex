
datatype dirType   = SRC | DST | ANY | UNI
datatype lexresult = EOF | TYPE of string | DIR of dirType | PROTO of string | OTHER_KEY of string |
                     ETHERNET of string | IPV4 of string | IPV6 of string | 
                     AND | OR | NOT | BIT_AND | BIT_OR | BIT_NOT |
                     ABOVE | LESS | EQUAL | ABOVE_OR_EQUAL | LESS_OR_EQUAL | NOT_EQUAL | 
                     SLASH | ADD | SUB | MUL | MOD | XOR | SHL | SHR |
                     LBRACKET | RBRACKET | INTEGER of int

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

fun printYytext text = print (text ^ "\n")

fun strToDirType str = case str of 
                          "dst" => DST
                        | "src" => SRC 
                        | "dst and src" => ANY
                        | "dst or src" => UNI
                        | _  => ANY

%%
%structure FilterLex
ws            = [\ \t]+;
l_bracket     = "(";
r_bracket     = ")";
and           = "and"|"&&";
or            = "or"|"||";
not           = "not"|"!";

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
dig           = [0-9];
nzdig         = [1-9];
dot           =  ".";
nonzerohex    = [1-9a-fA-F];
hexdigit      = [0-9a-fA-F];
semi          = ":";

dir           = "dst"|"src"|"dst and src"|"dst or src";
type          = "host"|"net"|"port";
proto         = "ether"|"fddi"|"tr"|"ip"|"ip6"|"arp"|"rarp"|"decnet"|"tcp"|"udp"|"icmp"|"icmp6";
other_kwd     = "gateway"|"proto"|"multicast"|"broadcast"|"less"|"greater";

ether_addr_seg = {hexdigit}{2};
ether_addr     = ({ether_addr_seg}{semi}){5}{ether_addr_seg};

ipv4_addr_seg  = {dig}{1,3};
ipv4_addr      = {ipv4_addr_seg}{dot}{ipv4_addr_seg}{dot}{ipv4_addr_seg}{dot}{ipv4_addr_seg};

omit_zeros     = {semi}{2};
ipv6_addr_seg  = {hexdigit}{4}|({nonzerohex}{hexdigit}{0,3})|"0";
ipv6_addr_full = ({ipv6_addr_seg}{semi}){7}{ipv6_addr_seg};
ipv6_addr_omit = (({ipv6_addr_seg}{semi}){0,6}{ipv6_addr_seg})?{omit_zeros}({ipv6_addr_seg}({semi}{ipv6_addr_seg}){0,6})?;
ipv6_addr      = {ipv6_addr_full}|{ipv6_addr_omit};

integer        = {nzdig}{dig}*;

%%
\n => (!linenum = !linenum + 1; lex());
{ws} => (lex());
{integer}  => ((printYytext yytext);
                INTEGER (case (Int.fromString yytext) of
                    SOME i => i
                 |  NONE   => 0));
{l_bracket} => (printYytext yytext; LBRACKET);
{r_bracket}  => (printYytext yytext; RBRACKET);
{and}  =>   (printYytext yytext; AND);
{or}   => (printYytext yytext; OR);
{not}  => (printYytext yytext; NOT);
{dir}   =>  (printYytext yytext; DIR (strToDirType yytext));
{type}  => (printYytext yytext; TYPE yytext);
{proto} =>  (printYytext yytext; PROTO yytext);
{other_kwd} =>  (printYytext yytext; OTHER_KEY yytext);
{ether_addr} =>  (printYytext yytext; ETHERNET yytext);
{ipv4_addr}  => (printYytext yytext; IPV4 yytext);
{ipv6_addr}  => (printYytext yytext; IPV6 yytext);
.     => (error ("ignoring bad character " ^ yytext); lex());
