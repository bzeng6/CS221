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
          | S.False => typeof(e, t3)
          | _ => raise Fail "cond: t1 must be a bool"
        )
      | S.Eq(S.Nat(t1), S.Nat(t2)) => T.Bool
      | S.Pair(t1, t2) => T.Product(typeof(e, t1), typeof(e, t2))
      | S.First(S.Pair(t1, t2)) => typeof(e, t1)
      | S.Second(S.Pair(t1, t2)) => typeof(e, t2)
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
      | _ => raise Fail "typeof: invalid sugary term"
    )

end
