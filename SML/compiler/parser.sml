structure Dec = MlLex.UserDeclarations

open Dec;

fun main () = 
let 
    val lexer = MlLex.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))
                            
    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   INTEGER i => print (Int.toString i)
                                 | ID id => print id
                                 | String str => print str
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
