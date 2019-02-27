 
(*val lexer = 
    let val inputLine = fn f =>
        let fun loop result = 
            let val c = TextIO.inputN (f,1)
                val result = c :: result
                in if String.size c = 0 orelse c = "\n" then
                    String.implode (rev result)
                    else loop result
            end
            in loop nil
        end
        in FilterLex.makeLexer(fn n => inputLine TextIO.stdIn)
    end*)

structure Dec = FilterLex.UserDeclarations

fun main () = 
let 
    val lexer = FilterLex.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))
                            
    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   Dec.TYPE => print ("TYPE " (*^ lexer.yytext*) ^ "\n")
                                 | Dec.DIR  => print ("DIR " (*^ lexer.yytext*) ^ "\n")
                                 | Dec.PROTO => print ("PROTO " (*^ lexer.yytext*) ^ "\n")
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
