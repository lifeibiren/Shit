open PreProSym

type lexresult = CPreProToken

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

(*fun makeToken sym text =
  let
    val _ = count text
  in
    sym (value, Position (!lineStart, !columnStart))
  end*)

fun tok sym text =
  let
    val _ = count text
  in
    CPreProToken (sym, text, Position (!lineStart, !columnStart))
  end

%%
%structure CPreProLexer

white_space = [\ \t\n\r\t];
digit = [0-9];
nodigit = [_a-zA-Z];

q_char = [^\"\\\n];
q_char_sequence = {q_char}+;

h_char = [^\n>];
h_char_sequence = {h_char}+;

header_name = ("<" {h_char_sequence} ">") | ("\"" {q_char_sequence} "\"");


identifier_nodigit = {nodigit};

identifier_ = {identifier_nodigit} ({identifier_nodigit} | {digit})* ;


number = ("."? {digit}+) | ({digit}+ "."? {digit}*);
pp_special_char = [eEpP] [\+\-];
pp_number = {number} ({identifier_nodigit}* | {pp_special_char} | {number})*;


simple_escape_sequence = "'" | "\"" | "\\?" | "\\" | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v";

octal_digit = [0-7];
octal_escape_sequence = "\\" {octal_digit}{1,3};

hex_digit = [0-9a-fA-F];
hex_escape_sequence = "\x" {hex_digit}+;

escape_sequence = {simple_escape_sequence} | {octal_escape_sequence} | {hex_escape_sequence};

c_char = {escape_sequence} | [^'\\\n];
c_char_sequence = {c_char}+;
char_constant = "L"? ' {c_char} ' ;


s_char = {escape_sequence} | [^\"\\\n];
s_char_sequence = {s_char}+;

string_literal = "L"? \" {s_char_sequence}* \";


punctuator =   "["   | "]"  | "("  | ")"   | "{"  | "}"  | "."  | "->" |
                "++" | "--" | "&"  | "*"   | "-"  | "~"  | "!"  |
                "/"  | "%"  | "<<" | ">>"  | "<"  | ">"  | "<=" | ">=" | "=="   | "!=" | "^"   | "|"   | "&&" | "||" |
                "?"  | ":"  | ";"  | "..." | "="  | "*=" | "/=" | "%=" | "+="   | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|=" |
                ","  | "#"  | "##" | "<:"  | ":>" | "<%" | "%>" | "%:" | "%:%:" ;


%%


{white_space}+        => ( tok (WhiteSpace yytext) yytext );

"#" [^\n]* "if"       => ( tok If yytext );
"#" [^\n]* "ifdef"    => ( tok Ifdef yytext );
"#" [^\n]* "ifndef"   => ( tok Ifndef yytext );
"#" [^\n]* "elif"     => ( tok Elif yytext );
"#" [^\n]* "endif"    => ( tok Endif yytext );
"#" [^\n]* "include"  => ( tok Include yytext );
"#" [^\n]* "undef"    => ( tok Undef yytext );
"#" [^\n]* "line"     => ( tok Line yytext );
"#" [^\n]* "error"    => ( tok Error yytext );
"#" [^\n]* "pragma"   => ( tok Pragma yytext );

{header_name}     => ( tok (HeaderName (String.substring(yytext, 1, String.size (yytext) - 2))) yytext );
{identifier_}     => ( tok (Identifier yytext) yytext );
{pp_number}       => ( tok PpNumber yytext );
{char_constant}   => ( tok (CharConstant (String.sub (yytext, 0))) yytext);
{string_literal}  => ( tok (StringLiteral yytext) yytext );
{punctuator}      => ( tok (Punctuator yytext) yytext );

.                 => ( count yytext; error ("ignoring bad character " ^ yytext); lex());
