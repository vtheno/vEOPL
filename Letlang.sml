(* load "Parser"; *)
open Parser
(* 
fun true_value expr = ()
fun eval_expr (expr,env) =
    case expr of
	(Var id) => apply_env (env,id)
      | If(e1,e2,e3) => (
	  if true_value (eval_expr e1)
	  then eval_expr e2
	  else eval_expr e3
      )
      | Let(id,value,body) => (
	let 
	in 
	end 
      )
*)
exception NoBindingFound of string
exception InvalidEnv
datatype 'a Enviorment = Empty
		      | ExtendEnv of string * 'a  * 'a Enviorment
fun apply_env (env:'a Enviorment,search_var:string) =
    case env of
	Empty => raise NoBindingFound search_var
      | ExtendEnv ( save_var,save_val,save_env ) => 
	 if save_var = search_var 
	 then save_val
	 else apply_env (save_env,search_var)
      | _ => raise InvalidEnv
fun valueOf (expr,env) =
    case expr of
	(Var id) => apply_env (env,id)
      | If(e1,e2,e3) => (
	  if valueOf (e1,env) = 1
	  then valueOf (e2,env)
	  else valueOf (e3,env)
      )
      | Let(id,value,body) => (
	let val v = valueOf(value,env)
	in valueOf (body,ExtendEnv (id,v,env))
	end 
      )
      | Binop("+",l,r) => 
	let val lv = valueOf(l,env)
	    val rv = valueOf(r,env)
	in 
	    lv + rv
	end
      | Binop("-",l,r) => 
	let val lv = valueOf(l,env)
	    val rv = valueOf(r,env)
	in 
	    lv - rv
	end
fun main () =
    let
	fun loop () = 
	    (print ">> ";
	     let 
		 val inp = TextIO.inputLine TextIO.stdIn
	     in 
		 print ("=> " ^ (Int.toString  (valueOf ((read inp) ,ExtendEnv("x",6,Empty) ) )   ) ^ "\n");
		 loop ()
	     end
	    )
    in 
	loop ()
    end 
val _ = main ()

	     
