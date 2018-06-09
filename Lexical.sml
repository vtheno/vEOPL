structure Lexical :> Lexical = 
struct
fun Mem x [] = false
  | Mem x (c::cs) = (x=c) orelse Mem x cs
fun Get x [] = []
  | Get x ((t,c)::cs) = if x=t then c else Get x cs
fun IsDigit x = "0" <= x andalso x <= "9"
fun IsLetter x =
    ("a" <= x andalso x <= "z") orelse
    ("A" <= x andalso x <= "Z") 
fun IsSeparator x =
    (x = " " orelse x = "\n" orelse x = "\t" orelse x = "\\n" orelse x ="\\t")
fun explode "" = []
  | explode str = List.map (Char.toString) (String.explode str)
fun implode [] = ""
  | implode ((x::xs):string list) = x ^ (implode xs)
fun IsNumber nil = false
  | IsNumber [x] = IsDigit x
  | IsNumber (x::xs) = IsDigit x andalso (IsNumber xs)
fun IsAlphaAux nil = false
  | IsAlphaAux [x] = IsDigit x orelse IsLetter x
  | IsAlphaAux (x::xs) = (IsDigit x orelse IsLetter x) andalso IsAlphaAux xs
fun IsAlpha nil = false
  | IsAlpha (x::xs) = if IsLetter x
		      then IsAlphaAux xs
		      else false
fun GetNumAux buf [] = (implode (List.rev buf) ,[])
  | GetNumAux buf (l as (x::l'))  = 
    if IsDigit x
    then GetNumAux (x::buf) l'
    else (implode (List.rev buf) ,l)
val GetNum = GetNumAux []
exception GetIdentErr
fun GetIdentAux buf [] = (implode (List.rev buf),[])
  | GetIdentAux buf (l as (x::l'))  = 
    if IsLetter x orelse IsDigit x
    then GetIdentAux (x::buf) l'
    else (implode (List.rev buf),l)
fun GetIdent (x::l) =
    if IsLetter x
    then GetIdentAux [x] l
    else raise GetIdentErr
  | GetIdent _ = raise GetIdentErr
fun GetTail p buf [] = (implode (List.rev buf),[])
  | GetTail p buf (l as (x::xs)) = 
    if p x 
    then GetTail p (x::buf) xs 
    else (implode (List.rev buf),l)
fun GetSymbol spectab tok [] = (tok,[])
  | GetSymbol spectab tok (l as x::xs)  = 
    if Mem x (Get tok spectab)
    then GetSymbol spectab (tok^x) xs
    else (tok,l)
exception GetNextTokenErr
fun GetNextToken spectab [x] = (x,[])
  | GetNextToken spectab (x::(l as c::cs)) = 
    if IsLetter x then GetTail (fn x => IsLetter x orelse IsDigit x) [x] l
    else if IsDigit x
    then GetTail IsDigit [x] l
    else if Mem c (Get x spectab)
    then GetSymbol spectab (implode [x,c]) cs
    else (x,l)
  | GetNextToken spectab _ = raise GetNextTokenErr
fun Tokenise spectab [] = []
  | Tokenise spectab (l as x::l') = 
    if IsSeparator x
    then Tokenise spectab l'
    else let val (t,l'') = GetNextToken spectab l
	 in t::(Tokenise spectab l'') end
val SpecTab = [("=",["<",">","="]),
	       (">",["<",">"]),
	       ("<",["<",">"]),
	       ("==",[">"])]
val Lex = (Tokenise SpecTab) o explode
end
