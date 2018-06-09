structure Parser :> Parser =
struct
open Lexical
exception ParseError of string 
fun strip ( tok , toks ) =
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
	      | Let of string * Expr * Expr
fun elem (target,nil) = false
  | elem (target,(x::xs)) = if target = x 
			    then true
			    else elem (target,xs)	    
fun parseIdentifier (first::rest) =
    if not (elem (first ,["if","then","else","let","=","in"]))
    then (first , rest)
    else raise ParseError "Identifier error"
  | parseIdentifier _ = raise ParseError "Identifier error"
fun parseAtom toks = 
    case toks of 
	("if"::rest) => (
	 let val (exp1,rest1) = parseExp rest
	     val (exp2,rest2) = parseExp (strip ("then",rest1))
	     val (exp3,rest3) = parseExp (strip ("else",rest2))
	 in 
	     (If (exp1,exp2,exp3) , rest3 )
	 end 
	 )
      | ("let"::rest) => 
	let val (id,rest1)    = parseIdentifier rest
	    val (value,rest2) = parseExp (strip ("=",rest1))
	    val (body,rest3)  = parseExp (strip ("in",rest2))
	in 
	    ( Let(id,value,body),rest3)
	end
      | ("("::rest) => (
	  let val (exp1,rest1) = parseExp rest
	  in (exp1,strip (")",rest1))
	  end )
      | rests => ( (* is Ident then parseIdentifier elif is Num then parseNum else ... *)
	  let val (var,rest) = parseIdentifier rests
	  in 
	      (Var var,rest)
	  end
      )
      (* | _ => raise ParseError "parseAtom" *)
and parseExp toks = 
    let val (exp1,rest1) = parseAtom toks
    in parseRest (exp1,rest1)
    end
and parseRest (exp1 ,toks ) =
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
val parse = parseExp o Lex
fun read inp =
    case parse inp of
	(result,nil) => result
      | _ => raise ParseError ("read: " ^ inp )
fun show expr =
    case expr of
	Var v => v
      | Binop (opname,left,right) => (show left) ^ " " ^ opname ^ " " ^ (show right)
      | If (exp1,exp2,exp3) => "if " ^ (show exp1) ^ " then " ^ (show exp2) ^ " else " ^ (show exp3)
      | Let (id,value,body) => "let " ^ id ^ "=" ^ (show value) ^ " in " ^ (show body)
end 
