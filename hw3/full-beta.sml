structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  (* fun step _ = raise Fail "todo: FullBeta.step" *)
  fun step term =
    (case term of
        ULC.Var(_) => NONE
      | ULC.App(t1, t2) => 
        (* check if can reduce t1, sub into app if so *)
        (case step(t1) of 
            SOME(t1') => SOME(ULC.App(t1', t2))
          | _ => 
            (* check if can reduce t2, sub into app if so *)
            (case step(t2) of
                SOME(t2') => SOME(ULC.App(t1, t2'))
              | _ =>
                (* check if t1 is abstr and can reduce term, sub if so *)
                (case t1 of
                    ULC.Lam(t1_s, t1_t) => 
                      (case step(t1_t) of
                          SOME(t1_t') => SOME(ULC.App(ULC.Lam(t1_s, t1_t'), t2))
                        (* (lamda t1_s.t1_t) t2 *)
                        | _ => SOME(Subst.subst(t1_s, t2, t1_t))
                      )
                  | _ => NONE (* var *)
                )
            )
        )
      | ULC.Lam(s1, t1) =>
        (* check if can reduce t1, sub into lam if so *)
        (case step(t1) of
            SOME(t1') => SOME(ULC.Lam(s1, t1'))
          (* don't call subst bc no input (not app) *)
          | _ => NONE
        )
    )

end
