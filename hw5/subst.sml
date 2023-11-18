structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  

  (* datatype term
    = Int of int
    | True
    | False
    | Unit
    | Var of string
    | Lam of string * Type.typ * term
    | App of term * term
    | Fix of term
    | Let of string * term * term
    | Cond of term * term * term
    | Add of term * term
    | Sub of term * term
    | Mul of term * term
    | Eq of term * term
    | LessThan of term * term
    | Not of term
    | Record of (string * term) list
    | Select of string * term *)

  (* fun subst (x, s, t) = raise Fail "todo: Subst.subst" *)
  (* subst(x,s,t) means "rewrite x to s in t" *)
  fun subst (x, s, t) =
    (case t of
        L.Int(n) => L.Int(n)
      | L.True => L.True
      | L.False => L.False
      | L.Unit => L.Unit
      | L.Var(y) =>
          if x = y then s else L.Var(y) 
      | L.Lam(y, tp, t1) =>
          if x = y then
            L.Lam(y, tp, t1)
          else
            L.Lam(y, tp, subst(x, s, t1))
      | L.App(t1, t2) => L.App(subst(x, s, t1), subst(x, s, t2))
      | L.Fix(t1) => L.Fix(subst(x, s, t1))
      | L.Let(y, t1, t2) =>
          if x = y then
            L.Let(y, subst(x, s, t1), t2)
          else
            L.Let(y, subst(x, s, t1), subst(x, s, t2))
      | L.Cond(t1, t2, t3) => L.Cond(subst(x, s, t1), subst(x, s, t2), subst(x, s, t3))
      | L.Add(t1, t2) => L.Add(subst(x, s, t1), subst(x, s, t2))
      | L.Sub(t1, t2) => L.Sub(subst(x, s, t1), subst(x, s, t2))
      | L.Mul(t1, t2) => L.Mul(subst(x, s, t1), subst(x, s, t2))
      | L.Eq(t1, t2) => L.Eq(subst(x, s, t1), subst(x, s, t2))
      | L.LessThan(t1, t2) => L.LessThan(subst(x, s, t1), subst(x, s, t2))
      | L.Not(t1) => L.Not(subst(x, s, t1))
      | L.Record(lst) =>
          let 
            fun subrec rlist =
              (case rlist of
                  (l1, t1)::rest => (l1, subst(x, s, t1))::subrec(rest)
                | _ => []
              )
          in
            L.Record(subrec(lst))
          end
      | L.Select(l, t1) => L.Select(l, subst(x, s, t1))
      (* | _ => raise Fail "todo" *)
    )

    (* U.App (tA, tB) => U.App (subst (x, t2, tA), subst (x, t2, tB))
        | U.Lam (y, tB) =>
            if x = y then U.Lam (y, tB)
            else if S.mem (y, fv t2) then subst (x, t2, fresh (y, tB))
            else U.Lam (y, subst (x, t2, tB))) *)
    (* (case t of
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
    ) *)

end
