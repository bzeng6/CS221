structure Desugar : sig

  (* helper *)
  val succ : Sweetl.term -> ULC.term

  val desugar : Sweetl.term -> ULC.term

end = struct

  fun succ (Sweetl.Nat(n)) = 
    (case n of 
        0 => ULC.Var("z")
      | 1 => ULC.App(ULC.Var("s"), ULC.Var("z"))
      | _ => ULC.App(ULC.Var("s"), succ(Sweetl.Nat(n - 1)))
    )

  (* fun desugar _ = raise Fail "todo: Desugar.desugar" *)
  fun desugar term =
    (case term of
        Sweetl.Var(t1) => ULC.Var(t1)
      | Sweetl.App(t1, t2) => ULC.App(desugar(t1), desugar(t2))
      | Sweetl.Lam(str1, t1) => ULC.Lam(str1, desugar(t1))
      | Sweetl.Nat(n) => ULC.Lam("s", ULC.Lam("z", succ(Sweetl.Nat(n))))
      | Sweetl.Tru => ULC.Lam("t", ULC.Lam("f", ULC.Var("t")))
      | Sweetl.Fls => ULC.Lam("t", ULC.Lam("f", ULC.Var("f")))
      | Sweetl.ID(t1) => ULC.Lam(t1, ULC.Var(t1))
      | Sweetl.Abbr(t1) => raise Fail "desugar: abbr should be unrolled"
      (* | _ => raise Fail "desugar: invalid Sweetl input" *)
    )

end
