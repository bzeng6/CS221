structure Desugar : sig

  val desugar : Sugary.term -> Desugared.term

end = struct

  structure S = Sugary
  structure D = Desugared

  (* fun desugar _ = raise Fail "todo" *)
  fun desugar (S.Nat(0)) = D.Zero
    | desugar (S.Nat(n)) = D.Succ(desugar(S.Nat(n - 1)))
    | desugar (S.True) = D.Succ(D.Zero)
    | desugar (S.False) = D.Zero
    | desugar (S.Unit) = D.Zero
    | desugar (S.Add(t1, t2)) = D.Add(desugar(t1), desugar(t2))
    | desugar (S.Subtract(t1, t2)) = D.Subtract(desugar(t1), desugar(t2))
    | desugar (S.Less(t1, t2)) = D.Less(desugar(t1), desugar(t2))

    | desugar (S.Greater(t1, t2)) = D.Less(desugar(t2), desugar(t1))
    | desugar (S.LessEq(t1, t2)) = 
      D.Cond(
        D.Less(desugar(t1), desugar(t2)),
        D.Succ(D.Zero), (* ?? *)
        D.Eq(desugar(t1), desugar(t2))
      )
    | desugar (S.GreaterEq(t1, t2)) = 
      D.Cond(
        D.Less(desugar(t2), desugar(t1)),
        D.Succ(D.Zero),
        D.Eq(desugar(t2), desugar(t1))
      )
    | desugar (S.Not(t1)) = 
      D.Cond(
        D.Eq(desugar(t1), D.Succ(D.Zero)),
        D.Zero,
        D.Succ(D.Zero)
      )
    | desugar (S.And(t1, t2)) = 
      D.Cond(
        D.Eq(desugar(t1), D.Succ(D.Zero)),
        D.Eq(desugar(t2), D.Succ(D.Zero)),
        D.Zero
      )
    | desugar (S.Or(t1, t2)) = 
      D.Cond(
        D.Eq(desugar(t1), D.Succ(D.Zero)),
        D.Succ(D.Zero),
        D.Eq(desugar(t2), D.Succ(D.Zero))
      )
    | desugar (S.Xor(t1, t2)) = 
      D.Cond(
        D.Eq(desugar(t1), D.Succ(D.Zero)),
        D.Eq(desugar(t2), D.Zero),
        D.Eq(desugar(t2), D.Succ(D.Zero))
      )  

    | desugar (S.Cond(t1, t2, t3)) = D.Cond(desugar(t1), desugar(t2), desugar(t3))
    | desugar (S.Eq(t1, t2)) = D.Eq(desugar(t1), desugar(t2))
    | desugar (S.Pair(t1, t2)) = D.Pair(desugar(t1), desugar(t2))
    | desugar (S.First(S.Pair(t1, _))) = D.First(desugar(t1))
    | desugar (S.Second(S.Pair(_, t2))) = D.Second(desugar(t2))
    | desugar _ = raise Fail "invalid type"
  
end
