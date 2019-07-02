structure Dec = CPreProLexer.UserDeclarations
open Dec;

fun genTokenList lexer: CPreProToken list =
  case lexer () of
    EOF => []
  | tok => tok :: genTokenList lexer

local
  fun doPrint x = case x of
        CPreProToken (WhiteSpace _, _, _) => ()

      | CPreProToken (sym, text, Position (row, column)) =>
         print ("extract token \"" ^ text ^ "\" at " ^
               "(" ^ (Int.toString row) ^ "," ^ (Int.toString column) ^ ")\n")
      | _ => ()
in
  fun printAllToken (x::xs) = (doPrint x; printAllToken xs)
    | printAllToken  _      = ()
end

fun makeConstExpr (x::xs) =
let
in
  case x of

end

fun makeCSource (x::xs) =
let
in
  case x of
    CPreProToken (If, _, _) =>
  | CPreProToken (Elif, _, _) =>
  | CPreProToken (Endif, _, _ ) =>
  | CPreProToken (Include, _, _) =>
  | CPreProToken (Pragma, _, _ ) =>
  | CPreProToken (Source, _, _) =>
end


fun main () =
let
    val input = fn n => TextIO.inputN(TextIO.stdIn, n)
    val lexer = CPreProLexer.makeLexer input
    val tokList = genTokenList lexer

in
    printAllToken tokList
end

val _ = main()
