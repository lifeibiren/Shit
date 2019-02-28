structure Dec = FilterLex.UserDeclarations

open Dec;

fun main () = 
let 
    val lexer = FilterLex.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))
                            
    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   TYPE => print ("TYPE " (*^ lexer.yytext*) ^ "\n")
                                 | Dec.DIR  => print ("DIR " (*^ lexer.yytext*) ^ "\n")
                                 | Dec.PROTO => print ("PROTO " (*^ lexer.yytext*) ^ "\n")
                                 | INTEGER i => print (Int.toString i)
                                 | _ => ()
                     in
                         if token <> Dec.EOF
                         then extract ()
                         else ()
                     end
in
    extract()
end

val _ = main()
