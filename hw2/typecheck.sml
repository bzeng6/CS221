structure TypeCheck : sig

  (* helper *)
  (* val isBool : Sugary.term -> bool *)

  val typeof : Sugary.term -> Type.typ

end = struct

  structure S = Sugary
  structure T = Type

  (* fun isBool n =
    if typeof(n) = T.Bool
    then true
    else false *)
    (* (case n of
        S.True => true
      | S.False => true
      | _ => false
    ) *)
  
  fun typeof (S.Nat(_)) = T.Nat
    | typeof (S.True) = T.Bool
    | typeof (S.False) = T.Bool
    | typeof (S.Unit) = T.Unit
    | typeof (S.Add(t1, t2)) = 
      if (typeof(t1) = T.Nat) andalso (typeof(t2) = T.Nat )
      then T.Nat 
      else raise Fail "add: type mismatch"
    | typeof (S.Subtract(t1, t2)) = 
      if (typeof(t1) = T.Nat) andalso (typeof(t2) = T.Nat )
      then T.Nat 
      else raise Fail "subtract: type mismatch"
    | typeof (S.Less(S.Nat(t1), S.Nat(t2))) = T.Bool
    | typeof (S.Greater(S.Nat(t1), S.Nat(t2))) = T.Bool
    | typeof (S.LessEq(S.Nat(t1), S.Nat(t2))) = T.Bool
    | typeof (S.GreaterEq(S.Nat(t1), S.Nat(t2))) = T.Bool
    | typeof (S.Not(t1)) = 
      if typeof(t1) = T.Bool
      then T.Bool 
      else raise Fail "not: type mismatch"
    | typeof (S.And(t1, t2)) = 
      if typeof(t1) = T.Bool
      then 
        if typeof(t2) = T.Bool
        then T.Bool 
        else raise Fail "and: t2 is not bool"
      else raise Fail "and: t1 is not bool"
    | typeof (S.Or(t1, t2)) = 
      if typeof(t1) = T.Bool
      then 
        if typeof(t2) = T.Bool
        then T.Bool 
        else raise Fail "or: t2 is not bool"
      else raise Fail "or: t1 is not bool"
    | typeof (S.Xor(t1, t2)) = 
      if typeof(t1) = T.Bool
      then 
        if typeof(t2) = T.Bool
        then T.Bool 
        else raise Fail "xor: t2 is not bool"
      else raise Fail "xor: t1 is not bool"
    | typeof (S.Cond(t1, t2, t3)) = 
      if typeof(t1) = T.Bool
      then 
        if (typeof(t2) = typeof(t3)) 
        then typeof(t2) 
        else raise Fail "cond: t2 and t3 are different types"
      else raise Fail "cond: t1 is not bool"
    | typeof (S.Eq(t1, t2)) = 
      if (typeof(t1) = typeof(t2)) 
      then T.Bool 
      else raise Fail "eq: t1 and t2 are different types"
    | typeof (S.Pair(t1, t2)) = T.Product(typeof(t1), typeof(t2))
    | typeof (S.First(t1)) =
      (case t1 of
          S.Pair(n1,_) => typeof(n1)
        | _ => raise Fail "first: type mismatch"
      )
    | typeof (S.Second(t1)) = 
      (case t1 of
          S.Pair(_, n2) => typeof(n2)
        | _ => raise Fail "second: type mismatch"
      )
    | typeof _ = raise Fail "typechecker: invalid type"
  
end
