structure Dec = CPreProLexer.UserDeclarations

open Dec;

fun main () =
let
    val lexer = CPreProLexer.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))

    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   Token (WhiteSpace _, _, _) => ()

                                 | Token (sym, text, Position (row, column)) =>
                                    print ("extract token \"" ^ text ^ "\" at " ^
                                          "(" ^ (Int.toString row) ^ "," ^ (Int.toString column) ^ ")\n")
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
