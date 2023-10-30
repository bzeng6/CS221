structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct
		  
  (* fun step _ = raise Fail "todo: Lazy.step" *)
  fun step term =
    (case term of
        ULC.App(t1, t2) => 
          (case t1 of
              ULC.Lam(t1_s, t1_t) => SOME(Subst.subst(t1_s, t2, t1_t))
            | _ => 
              (case step(t1) of
                  SOME(t1') => SOME(ULC.App(t1', t2))
                | _ => NONE
              )
          )
      | _ => NONE
    )

end
