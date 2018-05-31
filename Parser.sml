load "Lexical";
open Lexical
exception ParseError of string 
fun strip ( tok:string , toks:string list ) : string list=
    case toks of
	nil => raise ParseError "strip nil"
     | (first::rest) => (
	 if tok = first 
	 then rest
	 else raise ParseError "strip"
     )
datatype Expr = If of Expr * Expr * Expr
	      | Var of string 
	      | Binop of string * Expr * Expr

fun parseAtom (toks : string list) : Expr * string list = 
    case toks of 
	("if"::rest) => (
	 let val (exp1,rest1) = parseExp rest
	     val (exp2,rest2) = parseExp (strip ("then",rest1))
	     val (exp3,rest3) = parseExp (strip ("else",rest2))
	 in 
	     (If (exp1,exp2,exp3) , rest3 )
	 end 
	 )
      | ("("::rest) => (
	  let val (exp1,rest1) = parseExp rest
	  in (exp1,strip (")",rest1))
	  end )
      | (var::rest) => ( Var var,rest)
      | _ => raise ParseError "parseAtom"
and parseExp (toks : string list) : Expr * string list =
    let val (exp1,rest1) = parseAtom toks
    in parseRest (exp1,rest1)
    end
and parseRest (exp1 : Expr ,toks : string list) =
    case toks of
	("+"::rest1) => (
	 let
	     val (exp2,rest2) = parseAtom rest1
	 in 
	     parseRest (Binop ("+",exp1,exp2) , rest2)
	 end
	 )
      | ("-"::rest1) => (
	  let 
	      val (exp2,rest2) = parseAtom rest1
	  in 
	      parseRest (Binop ("-",exp1,exp2),rest2)
	  end
      )
      | _ => (exp1,toks)
val parse : string -> Expr * string list = parseExp o Lex
fun read (inp:string) : Expr =
    case parse inp of
	(result,nil) => result
      | _ => raise ParseError "read"
