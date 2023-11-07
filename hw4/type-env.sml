structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = (string * Type.typ) list
	       
  val empty = []

  (* fun lookup _ = raise Fail "todo" *)
  fun lookup (e, term) =
    (case e of
        ((str, tp)::rest) => 
          if term = str then
            SOME(tp)
          else
            lookup(rest, term)
      | _ => NONE
    )

  (* fun extend _ = raise Fail "todo" *)
  fun extend (e, term, tp) = ((term, tp)::e)
    (* (case lookup(e, term) of
        SOME(_) => e
      | _ => ((term, tp)::e)
    ) *)
					  
end
