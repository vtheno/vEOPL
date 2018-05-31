signature Lexical =
sig
    val IsDigit : string -> bool
    val IsLetter : string -> bool
    val IsSeparator : string -> bool
    val implode : string list -> string
    val explode : string -> string list
    val GetNumAux : string list -> string list -> string * string list
    val GetNum : string list -> string * string list
    val GetIdentAux : string list -> string list -> string * string list
    val GetIdent : string list -> string * string list
    val GetTail : (string -> bool) -> string list -> string list -> string * string list
    val Mem : ''a -> ''a list -> bool
    val Get : ''a -> (''a * 'b list) list -> 'b list
    val GetSymbol : (string * string list) list -> string -> string list -> string * string list
    val GetNextToken : (string * string list) list ->
		       string list -> string * string list
    val Tokenise : (string * string list) list ->
		   string list -> string list
    val SpecTab : (string * string list) list
    val Lex : string -> string list
end
