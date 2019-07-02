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
        CharConstant  of char   |
        StringLiteral of string |
        Punctuator    of string |
        WhiteSpace    of string

    (* row, column *)
    datatype Position = Position of int * int

    datatype CPreProToken = CPreProToken of
      CPreProSymbol * string * Position | EOF
end


datatype ConstExpr =
  EqualExpr of string * string

datatype CSource =
    If of ConstExpr * CSource
  | Elif of ConstExpr * CSource
  | Endif
  | Include of string
  | Pragma of string
  | Source of string
