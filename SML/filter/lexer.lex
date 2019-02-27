
datatype lexresult = TYPE | DIR | PROTO | IP of string | PORT of int | AND | OR | NOT | EOF

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => EOF

%%
%structure FilterLex
alpha=[A-za-z];
digit=[0-9];
ws = [\ \t];
%%
\n => (!linenum = !linenum + 1; lex());
{ws}+ => (lex());
"host" => (TYPE);
"net" => (TYPE);
"port" => (TYPE);
"portrange" => (TYPE);
"src" => (DIR);
"dst" => (DIR);
"src or dst" => (DIR);
"src and dst" => (DIR);
"ether" => (PROTO);
"wlan" => (PROTO);
"ip" => (PROTO);
"ip6" => (PROTO);
"arp" => (PROTO);
"rarp" => (PROTO);
"tcp" => (PROTO);
"udp" => (PROTO);
.     => (error ("ignoring bad character " ^ yytext); lex());

