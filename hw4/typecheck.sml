structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  (* fun typeof _ = raise Fail "todo" *)
  fun typeof (e, term) =
    (case term of
        S.Nat(_) => T.Nat
      | S.True => T.Bool
      | S.False => T.Bool
      | S.Unit => T.Unit
      | S.Add(S.Nat(_), S.Nat(_)) => T.Nat
      | S.Subtract(S.Nat(_), S.Nat(_)) => T.Nat
      | S.Mul(S.Nat(_), S.Nat(_)) => T.Nat
      | S.Pow(S.Nat(_), S.Nat(_)) => T.Nat
      | S.Less(S.Nat(_), S.Nat(_)) => T.Bool
      | S.Greater(S.Nat(_), S.Nat(_)) => T.Bool
      | S.LessEq(S.Nat(_), S.Nat(_)) => T.Bool
      | S.GreaterEq(S.Nat(_), S.Nat(_)) => T.Bool
      | S.Not(t1) =>
        (case t1 of
            S.True => T.Bool
          | S.False => T.Bool
          | _ => raise Fail "not: t1 must be a bool"
        )
      | S.And(t1, t2) =>
        (case t1 of
            S.True =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | S.False =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | _ => raise Fail "and: t1 must be a bool"
        )
      | S.Or(t1, t2) =>
        (case t1 of
            S.True =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | S.False =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | _ => raise Fail "and: t1 must be a bool"
        )
      | S.Xor(t1, t2) =>
        (case t1 of
            S.True =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | S.False =>
              (case t2 of
                  S.True => T.Bool
                | S.False => T.Bool
                | _ => raise Fail "and: t2 must be a bool"
              )
          | _ => raise Fail "and: t1 must be a bool"
        )
      | S.Cond(t1, t2, t3) =>
        (case t1 of
            S.True => typeof(e, t2)
              (* (case typeof(e, t2) of
                  t2_tp => t2_tp
                | _ => raise Fail "cond: t2 invalid sugary term"
              ) *)
          | S.False => typeof(e, t3)
              (* (case typeof(e, t3) of
                  t3_tp => t3_tp
                | _ => raise Fail "cond: t3 invalid sugary term"
              ) *)
          | _ => raise Fail "cond: t1 must be a bool"
        )
      | S.Eq(S.Nat(t1), S.Nat(t2)) => T.Bool
      | S.Pair(t1, t2) => T.Product(typeof(e, t1), typeof(e, t2))
        (* (case typeof(e, t1) of
            t1_tp => 
              (case typeof(e, t2) of
                  t2_tp => T.Product(t1_tp, t2_tp)
                | _ => raise Fail "pair: t2 invalid sugary term"
              )
          | _ => raise Fail "pair: t1 invalid sugary term"
        ) *)
      | S.First(S.Pair(t1, t2)) => typeof(e, t1)
        (* (case typeof(e, t1) of
            t1_tp => t1_tp
          | _ => raise Fail "first: t1 invalid sugary term"
        ) *)
      | S.Second(S.Pair(t1, t2)) => typeof(e, t2)
        (* (case typeof(e, t2) of
            t2_tp => t2_tp
          | _ => raise Fail "second: t2 invalid sugary term"
        ) *)
      | S.Var(x) => 
        (case TypeEnv.lookup(e, x) of
            SOME(tp) => tp
          | _ => raise Fail "var: lookup fail, term not found"
        )
      | S.Let(x, t1, t2) => 
        let
          val e_new = TypeEnv.extend(e, x, typeof(e, t1))
        in
          typeof(e_new, t2)
        end
        (* (case typeof(e, t1) of
            t1_tp =>
              (case TypeEnv.extend(e, x, t1_tp) of
                  e2 => 
                    (case typeof(e2, t2) of
                        t2_tp => t2_tp
                      | _ => raise Fail "let: t2 invalid sugary term"
                    )
                | _ => raise Fail "let: extend fail, could not add term to env"
              )
          | _ => raise Fail "let: t1 invalid sugary term"
        ) *)
      | _ => raise Fail "typeof: invalid sugary term"

        (* (case TypeEnv.lookup(e, x) of
            SOME(x_tp) => 
              (case typeof(t1) of
                  t1_tp => 
                    if x_tp = t1_tp then
                      typeof(t2)
                    else 
                      raise Fail "let: x and t1 different types"
                | _ => raise Fail "let: t1 is invalid type"
              )
          | _ => raise Fail "let: lookup fail, term not found"
        ) *)

    )

end
