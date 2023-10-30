structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct
		  
  (* fun fv _ = raise Fail "todo: Subst.fv" *)
  fun fv term =
    (case term of
        ULC.Var(s) => VarSet.ins(s, VarSet.empty)
      | ULC.App(t1, t2) => VarSet.union(fv(t1), fv(t2))
      | ULC.Lam(x, t1) => VarSet.rem(x, fv(t1))
    )
		  
  (* fun subst _ = raise Fail "todo: Subst.subst" *)
  (* subst(x,s,t) means "rewrite x to s in t" *)
  fun subst (x, s, t) =
    (case t of
        ULC.Var(y) =>
          if x = y then 
            s
          else 
            ULC.Var(y)
      | ULC.App(t1, t2) => ULC.App(subst(x, s, t1), subst(x, s, t2))
      | ULC.Lam(y, t1) =>
          if x = y then
            ULC.Lam(x, t1)
          else
            if VarSet.mem(y, fv(s)) then
              let
                val fresh_var = Fresh.var()
              in
                subst(x, s, ULC.Lam(fresh_var, subst(y, ULC.Var(fresh_var), t1)))
              end
            else
              ULC.Lam(y, subst(x, s, t1))
    )


end