structure Dec = FilterLex.UserDeclarations

open Dec;

fun main () = 
let 
    val lexer = FilterLex.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))
                            
    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   TYPE str => print ("TYPE " (*^ lexer.yytext*) ^ "\n")
                                 | DIR str => print ("DIR " ^ str ^ "\n")
                                 | PROTO str => print ("PROTO " (*^ lexer.yytext*) ^ "\n")
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
