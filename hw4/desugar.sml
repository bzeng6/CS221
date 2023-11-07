structure Desugar : sig

  (* helper *)
  val succ : Sugary.term -> ULC.term

  val desugar : Sugary.term -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  fun succ term =
    (case term of
        (S.Nat(n)) =>
          (case n of
              0 => U.Var("z")
            | 1 => U.App(U.Var("s"), U.Var("z"))
            | _ => U.App(U.Var("s"), succ(S.Nat(n - 1)))
          )
      | _ => raise Fail "succ: input should only be Nat"
    )
  
  (* fun desugar _ = raise Fail "todo" *)
  fun desugar term =
    let
      val tru = U.Lam("t", U.Lam("f", U.Var("t")))

      val fls = U.Lam("t", U.Lam("f", U.Var("f")))

      val pair = U.Lam("f", U.Lam("s", U.Lam("b", 
                                              U.App(U.App(U.Var("b"), U.Var("f")),
                                                    U.Var("s")))))

      val fst = U.Lam("p", 
                      U.App(U.Var("p"), 
                            U.Lam("t", U.Lam("f", U.Var("t")))))

      val snd = U.Lam("p", 
                      U.App(U.Var("p"), 
                            U.Lam("t", U.Lam("f", U.Var("f")))))

      val plus = U.Lam("m", U.Lam("n", U.Lam("s", U.Lam("z", 
                                                        U.App(U.App(U.Var("m"), U.Var("s")), 
                                                              U.App(U.App(U.Var("n"), 
                                                                          U.Var("s")), 
                                                                    U.Var("z")))))))

      val zz = U.App(U.App(pair,
                          U.Lam("s", U.Lam("z", U.Var("z")))),
                    U.Lam("s", U.Lam("z", U.Var("z"))))

      val ss = U.Lam("p", U.App(U.App(pair,
                                      U.App(snd, 
                                            U.Var("p"))),
                                U.App(U.App(plus,
                                            U.Lam("s", U.Lam("z", U.App(U.Var("s"),
                                                                       U.Var("z"))))),
                                      U.App(snd, U.Var("p")))))

      val prd = U.Lam("m", U.App(fst, 
                                U.App(U.App(U.Var("m"), 
                                            ss), 
                                      zz)))

      val minus = U.Lam("m", U.Lam("n", U.App(U.App(U.Var("n"),
                                                    prd),
                                              U.Var("m"))))

      val times = U.Lam("m", U.Lam("n", U.App(U.App(U.Var("m"),
                                                    U.App(plus, U.Var("n"))),
                                              U.Lam("s", U.Lam("z", U.Var("z"))))))                                    

      val power = U.Lam("m", U.Lam("n", U.App(U.App(U.Var("n"), 
                                                    U.App(times, 
                                                          U.Var("m"))),
                                              U.Lam("s", U.Lam("z", U.App(U.Var("s"), 
                                                                          U.Var("z")))))))

      val iszro = U.Lam("m", U.App(U.App(U.Var("m"),
                                          U.Lam("x", U.Lam("t", U.Lam("f", U.Var("f"))))),
                                    U.Lam("t", U.Lam("f", U.Var("t")))))

      val not = U.Lam("b", U.App(U.App(U.Var("b"), 
                                        fls),
                                  tru))
      
      val also = U.Lam("b", U.Lam("c", U.App(U.App(U.Var("b"),
                                                  U.Var("c")),
                                            fls)))                        

      val other = U.Lam("b", U.Lam("c", U.App(U.App(U.Var("b"),
                                                  tru),
                                            U.Var("c"))))

      val xor = U.Lam("b", U.Lam("c", U.App(U.App(U.Var("b"),
                                                  U.App(not, 
                                                        U.Var("c"))),
                                            U.Var("c"))))
      
      val eq = U.Lam("m", U.Lam("n", U.App(U.App(also,
                                                  U.App(iszro,
                                                        U.App(U.App(U.Var("m"),
                                                                    prd),
                                                              U.Var("n")))),
                                            U.App(iszro,
                                                  U.App(U.App(U.Var("n"),
                                                              prd),
                                                        U.Var("m"))))))

      val less = U.Lam("m", U.Lam("n", U.App(not, 
                                              U.App(iszro, 
                                                    U.App(U.App(minus, 
                                                                U.Var("n")), 
                                                          U.Var("m"))))))

      val greater = U.Lam("m", U.Lam("n", U.App(not,
                                                U.App(iszro,
                                                      U.App(U.App(minus,
                                                                  U.Var("m")),
                                                            U.Var("n"))))))

      val leq = U.Lam("m", U.Lam("n", U.App(U.App(other,
                                                      U.App(U.App(less,
                                                                  U.Var("m")),
                                                            U.Var("n"))),
                                                U.App(U.App(eq,
                                                            U.Var("m")),
                                                      U.Var("n")))))
      
      val geq = U.Lam("m", U.Lam("n", U.App(U.App(other,
                                                      U.App(U.App(greater,
                                                                  U.Var("m")),
                                                            U.Var("n"))),
                                                U.App(U.App(eq,
                                                            U.Var("m")),
                                                      U.Var("n")))))
      
      val test = U.Lam("l", U.Lam("m", U.Lam("n", U.App(U.App(U.Var("l"),
                                                              U.Var("m")),
                                                        U.Var("n")))))
      
    in
      (case term of
          S.Nat(n) => U.Lam("s", U.Lam("z", succ(S.Nat(n))))
        | S.True => tru
        | S.False => fls
        | S.Unit => U.Lam("s", U.Lam("z", U.Var("z")))
        | S.Add(t1, t2) => U.App(
            U.App(plus, desugar(t1)), 
            desugar(t2)
          )
        | S.Subtract(t1, t2) => U.App(
            U.App(minus, desugar(t1)),
            desugar(t2)
          )
        | S.Mul(t1, t2) => U.App(
            U.App(times, desugar(t1)),
            desugar(t2)
          )
        | S.Pow(t1, t2) => U.App(
            U.App(power, desugar(t1)),
            desugar(t2)
          )
        | S.Less(t1, t2) => U.App(
            U.App(less, desugar(t1)),
            desugar(t2)
          )
        | S.Greater(t1, t2) => U.App(
            U.App(greater, desugar(t1)),
            desugar(t2)
          )
        | S.LessEq(t1, t2) => U.App(
            U.App(leq, desugar(t1)),
            desugar(t2)
          )
        | S.GreaterEq(t1, t2) => U.App(
            U.App(geq, desugar(t1)),
            desugar(t2)
          )
        | S.Not(t1) => U.App(
            not, 
            desugar(t1)
          )
        | S.And(t1, t2) => U.App(
            U.App(also, desugar(t1)), 
            desugar(t2)
          )
        | S.Or(t1, t2) => U.App(
            U.App(other, desugar(t1)), 
            desugar(t2)
          )
        | S.Xor(t1, t2) => U.App(
            U.App(xor, desugar(t1)), 
            desugar(t2)
          )
        | S.Cond(t1, t2, t3) => U.App(
            U.App(U.App(test, desugar(t1)),
                  desugar(t2)),
            desugar(t3)
          )
        | S.Eq(t1, t2) => U.App(
            U.App(eq, desugar(t1)),
            desugar(t2)
          )
        | S.Pair(t1, t2) => U.App(
            U.App(pair, desugar(t1)),
            desugar(t2)
          )
        | S.First(pr) => U.App(
            fst, 
            desugar(pr)
          )
        | S.Second(pr) => U.App(
            snd,
            desugar(pr)
          )
        | S.Var(s) => U.Var(s)
        | S.Let(x, t1, t2) => U.App(U.Lam(x, desugar(t2)), desugar(t1))
      )
    end

end
