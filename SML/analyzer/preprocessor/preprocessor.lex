(*
datatype CPreProSymbol =
    If                      |
    Ifdef                   |
    Ifndef                  |
    Elif                    |
    Endif                   |
    Include                 |
    Define                  |
    Undef                   |
    Line                    |
    Error                   |
    Pragma                  |
    HeaderName    of string |
    Identifier    of string |
    PpNumber                |
    CharConstant            |
    StringLiteral of string |
    Punctuator    of string |
    WhiteSpace    of string
*)

datatype Position = Position of int * int (* row, column *)

(*
datatype Token = Token of CPreProSymbol * string * Position | EOF
type lexresult = Token
*)
structure Tokens = Tokens

type svalue = Tokens.svalue
type pos = Position
type lexresult = (svalue, pos) Tokens.token
type ('a,'b) token = ('a,'b) Tokens.token

val linenum = ref 1
val column = ref 1
val lineStart = ref 1
val columnStart = ref 1
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")

val eof = fn () => Tokens.EOF (Position (!lineStart, !columnStart),
                               Position (!lineStart, !columnStart))

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

fun makeToken sym value text =
  let
    val _ = count text
  in
    sym (value, Position (!lineStart, !columnStart), Position (!lineStart, !columnStart))
  end

fun tok sym text =
  let
    val _ = count text
  in
    sym (Position (!lineStart, !columnStart), Position (!lineStart, !columnStart))
  end
%%
%header (functor PreProParserLexFun(structure Tokens: PreProParser_TOKENS));


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


{white_space}+        => ( count yytext; lex() );

"#" [^\n]* "if"       => ( tok Tokens.IF yytext );
"#" [^\n]* "ifdef"    => ( tok Tokens.IFDEF yytext );
"#" [^\n]* "ifndef"   => ( tok Tokens.IFNDEF yytext );
"#" [^\n]* "elif"     => ( tok Tokens.ELIF yytext );
"#" [^\n]* "endif"    => ( tok Tokens.ENDIF yytext );
"#" [^\n]* "include"  => ( tok Tokens.INCLUDE yytext );
"#" [^\n]* "undef"    => ( tok Tokens.UNDEF yytext );
"#" [^\n]* "line"     => ( tok Tokens.LINE yytext );
"#" [^\n]* "error"    => ( tok Tokens.ERROR yytext );
"#" [^\n]* "pragma"   => ( tok Tokens.PRAGMA yytext );

{header_name}     => ( makeToken
                        Tokens.HEADER_NAME (String.substring(yytext, 1, String.size (yytext) - 2))
                        yytext );
{identifier_}     => ( makeToken Tokens.IDENTIFIER yytext yytext );
{pp_number}       => ( tok Tokens.PP_NUMBER yytext );
{char_constant}   => ( makeToken Tokens.CHAR_CONSTANT (String.sub (yytext, 0)) yytext );
{string_literal}  => ( makeToken Tokens.STRING_LITERAL yytext yytext );
{punctuator}      => ( makeToken Tokens.PUNCTUATOR yytext yytext );

.                 => ( count yytext; error ("ignoring bad character " ^ yytext); lex());
