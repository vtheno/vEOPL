fun IsDigit x = "0" <= x andalso x <= "9"
fun IsLetter x =
    ("a" <= x andalso x <= "z") orelse
    ("A" <= x andalso x <= "Z") 
fun explode "" = []
  | explode str = List.map (Char.toString) (String.explode str)
fun implode [] = ""
  | implode ((x::xs):string list) = 
    x ^ (implode xs)
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

