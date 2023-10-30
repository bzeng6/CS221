structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term

end = struct

  (* fun unroll _ = raise Fail "todo: Unroll.unroll" *)
  fun unroll prog = 
    (case prog of
        Sweetl.Prog(bindings, rolled) =>
          (case bindings of
              ((term, abbr)::rest) =>
                  (case rolled of
                      Sweetl.Abbr(r) => 
                        if r = term then
                          (case abbr of 
                              Sweetl.Abbr(_) => abbr
                            | x => unroll(Sweetl.Prog(bindings, x))
                          )
                        else
                          (case unroll(Sweetl.Prog(rest, rolled)) of
                              Sweetl.Abbr(s) =>
                                if s = term then
                                  (case abbr of
                                      Sweetl.Abbr(_) => abbr
                                    | _ => unroll(Sweetl.Prog(bindings, abbr))
                                  )
                                else
                                  Sweetl.Abbr(s)
                              | x => unroll(Sweetl.Prog(bindings, x))
                          )
                          (* outermost recursive call cannot return an abbreviation? *)
                    | Sweetl.Lam(t1, t2) => Sweetl.Lam(t1, unroll(Sweetl.Prog(bindings, t2)))
                    | Sweetl.App(t1, t2) => Sweetl.App(
                                              unroll(Sweetl.Prog(bindings, t1)), 
                                              unroll(Sweetl.Prog(bindings, t2))
                                            )
                    | _ => rolled                    
                  )
            | _ => rolled (* None? *)  
          )
      (* | _ => raise Fail "unroll: input to unroll should be a Sweetl.prog" *)
    )
				 
end
