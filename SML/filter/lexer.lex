
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val pos = ref 0
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => Tokens.EOF (!linenum, !pos)

fun printYytext text = print (text ^ "\n")

open Tokens
%%
%header (functor FilterLexFun(structure Tokens: Filter_TOKENS));
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
{and}  =>   (printYytext yytext; AND(!linenum, yypos));
{or} => (printYytext yytext; OR(!linenum, yypos));
"(" => (printYytext yytext; LBRACKET(!linenum, yypos));
")" => (printYytext yytext; RBRACKET(!linenum, yypos));
"src" => (printYytext yytext; SRC(!linenum, yypos));
"dst" => (printYytext yytext; DST(!linenum, yypos));
"host" => (printYytext yytext; HOST(!linenum, yypos));
"ip" => (printYytext yytext; IP(!linenum, yypos));
"port" => (printYytext yytext; PORT(!linenum, yypos));
"tcp" => (printYytext yytext; TCP(!linenum, yypos));
"vlan" => (printYytext yytext; VLAN(!linenum, yypos));
{integer} => (printYytext yytext; INTEGER((fn SOME i => i | NONE => 0) (Int.fromString yytext), !linenum, yypos));
{ipv4_addr} => (printYytext yytext; IPV4 (yytext, !linenum, yypos));
.     => (error ("ignoring bad character " ^ yytext); lex());
