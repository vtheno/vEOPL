open Parser
fun main () =
    let
	fun loop () = 
	    (print ">> ";
	     let 
		 val inp = TextIO.inputLine TextIO.stdIn
	     in 
		 print ("=> " ^ (show  (read inp)   ) ^ "\n");
		 loop ()
	     end
	    )
    in 
	loop ()
    end 
val _ = main ()

	     
