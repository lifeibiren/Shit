
datatype lexresult = BRACKET | TYPE | DIR | PROTO | AND | OR | NOT | OTHER_KEY | ETHERNET | IPV4 | IPV6 | EOF | INTEGER of int

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

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
{integer}  => (print ("integer " ^ yytext ^ "\n");
                INTEGER (case (Int.fromString yytext) of
                    SOME i => i
                 |  NONE   => 0));
{l_bracket} => (print ("l_bracket " ^ yytext ^ "\n"); BRACKET);
{r_bracket}  => (print ("r_bracket " ^ yytext ^ "\n"); BRACKET);
{and}  =>   (print ("and " ^ yytext ^ "\n"); AND);
{or}   => (print ("or " ^ yytext ^ "\n"); OR);
{not}  => (print ("not " ^ yytext ^ "\n"); NOT);
{dir}   =>  (print ("dir " ^ yytext ^ "\n"); DIR);
{type}  => (print ("type " ^ yytext ^ "\n"); TYPE);
{proto} =>  (print ("proto " ^ yytext ^ "\n"); PROTO);
{other_kwd} =>  (print ("other_kwd " ^ yytext ^ "\n"); OTHER_KEY);
{ether_addr} =>  (print ("ether_addr " ^ yytext ^ "\n"); ETHERNET);
{ipv4_addr}  => (print ("ipv4_addr " ^ yytext ^ "\n"); IPV4);
{ipv6_addr}  => (print ("ipv6_addr " ^ yytext ^ "\n"); IPV6);
.     => (error ("ignoring bad character " ^ yytext); lex());
