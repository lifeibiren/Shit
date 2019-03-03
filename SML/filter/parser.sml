(* structure Dec = FilterLex.UserDeclarations *)

(* open Dec; *)

structure FilterLrVals = FilterLrValsFun(structure Token = LrParser.Token)

structure FilterLex = FilterLexFun(structure Tokens = FilterLrVals.Tokens)

structure FilterParser = Join(structure Lex= FilterLex
                              structure LrParser = LrParser
                              structure ParserData = FilterLrVals.ParserData)
                              
(*fun main () = 
let 
    val lexer = FilterLex.makeLexer(fn n => TextIO.inputN(TextIO.stdIn, n))
                            
    fun extract () = let
                         val token = lexer()
                         val _ = case token of
                                   TYPE str => print ("TYPE " ^ "\n")
                                 | DIR str => print ("DIR " ^ str ^ "\n")
                                 | PROTO str => print ("PROTO " ^ "\n")
                                 | INTEGER i => print (Int.toString i)
                                 | _ => ()
                     in
                         if token <> Dec.EOF
                         then extract ()
                         else ()
                     end
in
    extract()
end*)


fun main() = 
let
    fun keybd () =
    let val stream =
            FilterParser.makeLexer (fn i => (case TextIO.inputLine TextIO.stdIn
                                              of SOME s => s
                                               | _ => ""))
        fun error (e,i:int,_) =
            TextIO.output(TextIO.stdOut,
              "std_in," ^ " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
     in FilterLex.UserDeclarations.linenum := 1;
        FilterParser.parse(0,stream,error,())
    end
in
    keybd()
end

val _ = main()
