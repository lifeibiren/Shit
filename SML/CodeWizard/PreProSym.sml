structure PreProSym = 
struct
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

    (* row, column *)
    datatype Position = Position of int * int 

    datatype CPreProToken = Token of CPreProSymbol * string * Position | EOF
end
