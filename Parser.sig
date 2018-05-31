signature Parser = 
sig
    exception ParseError of string 
    val strip : string * string list -> string list
    datatype Expr = If of Expr * Expr * Expr
		  | Var of string 
		  | Binop of string * Expr * Expr
		  | Let of string * Expr * Expr
    val parseAtom : string list -> Expr * string list
    val parseExp : string list -> Expr * string list
    val parseRest : Expr * string list -> Expr * string list
    val parse : string -> Expr * string list 
    val read : string -> Expr
    val show : Expr -> string
    val elem : ''a * ''a list -> bool
    val parseIdentifier : string list -> string * string list
end 
