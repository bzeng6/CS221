structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
      val _ = Check.expect (Scan.scan "ZZ", [T.Z, T.Z], "scan1")
      val _ = Check.expect (Scan.scan "Z  Z", [T.Z, T.Z], "scan2")
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan3")
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.Plus, T.Z, T.RBrack], A.Add(A.Zero, A.Zero), "parse1")
      val _ = Check.expect (Parse.parse [T.LBrack, T.T, T.Plus, T.F, T.RBrack], A.Add(A.True, A.False), "parse2")
      val _ = Check.expect (Parse.parse [T.LBrack, 
                                          T.LBrack, T.T, T.Plus, T.F, T.RBrack, 
                                          T.Plus, 
                                          T.LBrack, T.T, T.Plus, T.F, T.RBrack, 
                                        T.RBrack], 
                            A.Add(A.Add(A.True, A.False), A.Add(A.True, A.False)), "parse3")
      (* val _ = Check.expect (Parse.parse "[T.T, T.Plus, T.F]", A.Add(A.True, A.False), "parse0")       *)
      val _ = Check.exn (fn () => Parse.parse [T.T, T.Plus, T.F], "badParse1")
      val _ = Check.expect (Parse.parse [T.LBrack, T.LBrack, T.Z, T.Plus, T.Z, T.RBrack, T.Minus, T.P, T.Z, T.RBrack], A.Subtract (A.Add (A.Zero, A.Zero), A.Pred A.Zero), "parse4")    
    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      val _ = Check.expect (Eval.eval (A.Pred A.Zero), [A.Pred A.Zero, A.Zero], "eval1")
      (* val _ = Check.expect (Eval.eval (A.Pred (A.Succ (A.Or (A.True, A.False)))), [A.Pred (A.Succ (A.Or (A.True, A.False))), (A.Pred (A.Succ A.True))], "eval7") *)
      (* val _ = Check.expect (Eval.eval (A.Or (A.True, A.False)), [A.Or (A.True, A.False), A.True], "eval2")
      val _ = Check.expect (Eval.eval (A.Succ (A.Or (A.True, A.False))), [A.Succ (A.Or (A.True, A.False)), A.Succ(A.True)], "eval5")
      val _ = Check.expect (Eval.eval (A.Pred (A.Or (A.True, A.False))), [A.Pred (A.Or (A.True, A.False)), A.Pred(A.True)], "eval6") *)
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
      val _ = Check.expect (Compile.code "[T || [T && T]]", [A.Or (A.True, A.And (A.True, A.True)), A.True], "compile1")
      val _ = Check.expect (Compile.code "SPZ", [A.Succ (A.Pred A.Zero), A.Succ A.Zero], "compile2")
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
