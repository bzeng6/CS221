structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
      val _ = Check.expect (Scan.scan "ZZ", [T.Z, T.Z], "scan0")
      val _ = Check.expect (Scan.scan "Z  Z", [T.Z, T.Z], "scan0")
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.expect (Scan.scan "PSPSPSP[Z", [T.P, T.S, T.P, T.S, T.P, T.S, T.P, T.LBrack, T.Z], "scan1")
      val _ = Check.expect(Scan.scan "[Z+Z]", [T.LBrack, T.Z, T.Plus, T.Z, T.RBrack], "scan2")
      val _ = Check.expect(Scan.scan "[T&&F]", [T.LBrack, T.T, T.DoubleAmpersand, T.F, T.RBrack], "scan3")
      val _ = Check.expect(Scan.scan "\nZ", [T.Z], "scan4")
      val _ = Check.expect(Scan.scan "", [], "scan4")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      val _ = Check.exn (fn () => Scan.scan "PS(", "badScan01")
      val _ = Check.exn (fn () => Scan.scan "*", "badScan02")
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
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.expect (Parse.parse [T.S, T.S, T.Z], A.Succ (A.Succ A.Zero), "parse1")
      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.Plus, T.Z, T.RBrack], A.Add (A.Zero, A.Zero), "parse2")
      val _ = Check.expect (Parse.parse [T.LBrack, T.P, T.P, T.Z, T.Plus, T.P, T.Z, T.RBrack], A.Add (A.Pred (A.Pred A.Zero), A.Pred A.Zero), "parse3")
      val _ = Check.expect (Parse.parse [T.LBrack, T.P, T.P, T.Z, T.Minus, T.P, T.Z, T.RBrack], A.Subtract (A.Pred (A.Pred A.Zero), A.Pred A.Zero), "parse4")
      val _ = Check.expect (Parse.parse [T.LBrack, T.LBrack, T.Z, T.Plus, T.Z, T.RBrack, T.Minus, T.P, T.Z, T.RBrack], A.Subtract (A.Add (A.Zero, A.Zero), A.Pred A.Zero), "parse5")
      val _ = Check.expect (Parse.parse [T.LBrack, T.T, T.QuestionMark, T.Z, T.Colon, T.P, T.Z, T.RBrack], A.Cond (A.True, A.Zero, A.Pred A.Zero), "parse6")
      val _ = Check.expect (Parse.parse [T.LBrack, T.T, T.DoubleAmpersand, T.F, T.RBrack], A.And (A.True, A.False), "parse7")
      val _ = Check.expect (Parse.parse [T.P,T.LBrack,T.S,T.Z,T.Plus,T.S,T.Z,T.RBrack],  A.Pred (A.Add (A.Succ A.Zero,A.Succ A.Zero)), "parse8")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")

    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      val _ = Check.expect (Eval.eval (A.Pred A.Zero), [A.Pred A.Zero, A.Zero], "eval1")
      (* val _ = Check.expect (Eval.eval (A.Pred (A.Succ (A.Or (A.True, A.False)))), [A.Pred (A.Succ (A.Or (A.True, A.False))), (A.Pred (A.Succ A.True))], "eval2") *)
      val _ = Check.expect (Eval.eval (A.Or (A.True, A.False)), [A.Or (A.True, A.False), A.True], "eval2")
      val _ = Check.expect (Eval.eval (A.Succ (A.Or (A.True, A.False))), [A.Succ (A.Or (A.True, A.False)), A.Succ(A.True)], "eval5")
      val _ = Check.expect (Eval.eval (A.Pred (A.Or (A.True, A.False))), [A.Pred (A.Or (A.True, A.False)), A.Pred(A.True)], "eval6")
      val _ = Check.expect (Eval.eval (A.Add (A.Succ A.Zero, A.Succ A.Zero)), [A.Add (A.Succ A.Zero,A.Succ A.Zero),A.Add (A.Zero, A.Succ (A.Succ A.Zero)),A.Succ (A.Succ A.Zero)], "eval3")
      val _ = Check.expect (Eval.eval (A.Pred (A.Add (A.Succ A.Zero,A.Succ A.Zero))), [A.Pred (A.Add (A.Succ A.Zero,A.Succ A.Zero)),A.Pred (A.Add (A.Zero,A.Succ (A.Succ A.Zero))), A.Pred (A.Succ (A.Succ A.Zero)),A.Succ A.Zero], "eval4")
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
      val _ = Check.expect (Compile.code "[T || [T && T]]", [A.Or (A.True, A.And (A.True, A.True)), A.True], "compile1")
      val _ = Check.expect (Compile.code "SPZ", [A.Succ (A.Pred A.Zero), A.Succ A.Zero], "compile2")
      val _ = Check.expect (Compile.code "PSZ", [A.Pred (A.Succ A.Zero), A.Zero], "compile3")
      val _ = Check.expect (Compile.code "P[SZ + SZ]", [A.Pred (A.Add (A.Succ A.Zero,A.Succ A.Zero)),A.Pred (A.Add (A.Zero,A.Succ (A.Succ A.Zero))), A.Pred (A.Succ (A.Succ A.Zero)),A.Succ A.Zero], "compile3")
      val _ = Check.expect (Compile.code "[P[SZ + SZ] > SSZ]",   [A.Greater (A.Pred (A.Add (A.Succ A.Zero,A.Succ A.Zero)),A.Succ (A.Succ A.Zero)),
                                                                  A.Greater (A.Pred (A.Add (A.Zero,A.Succ (A.Succ A.Zero))),A.Succ (A.Succ A.Zero)),
                                                                  A.Greater (A.Pred (A.Succ (A.Succ A.Zero)),A.Succ (A.Succ A.Zero)),
                                                                  A.Greater (A.Succ A.Zero,A.Succ (A.Succ A.Zero)),A.Greater (A.Zero,A.Succ A.Zero),A.False], "compile4")
                                                          
      val _ = Check.expect (Compile.code "[Z + Z]", [A.Add (A.Zero, A.Zero), A.Zero], "compile5")
      val _ = Check.expect (Compile.code "[Z - SZ]", [A.Subtract (A.Zero, A.Succ A.Zero), A.Zero], "compile6")   
      val _ = Check.expect (Compile.code "[SST - SZ]", [A.Subtract (A.Succ (A.Succ A.True), A.Succ A.Zero)], "compile7")
      val _ = Check.expect (Compile.code "[SS[T || F] - SZ]", [A.Subtract (A.Succ (A.Succ (A.Or (A.True, A.False))), A.Succ A.Zero), A.Subtract (A.Succ (A.Succ A.True), A.Succ A.Zero)], "compile8")     
      val _ = Check.expect (Compile.code "[T > [Z + Z]]", [A.Greater (A.True, A.Add (A.Zero, A.Zero)), A.Greater (A.True, A.Zero)], "compile9")   
      val _ = Check.expect (Compile.code "[T < [Z + Z]]", [A.Less (A.True, A.Add (A.Zero, A.Zero)), A.Less (A.True, A.Zero)], "compile10")   
      val _ = Check.expect (Compile.code "[T ? SZ :Z]", [A.Cond (A.True, A.Succ A.Zero, A.Zero), A.Succ A.Zero], "compile11")                                               
      val _ = Check.expect (Compile.code "[F ? SZ :Z]", [A.Cond (A.False, A.Succ A.Zero, A.Zero), A.Zero], "compile12")  
      val _ = Check.expect (Compile.code "[T ? [Z + SZ] :Z]", [A.Cond (A.True, A.Add (A.Zero, A.Succ A.Zero), A.Zero), A.Add (A.Zero, A.Succ A.Zero), A.Succ A.Zero], "compile13")                                               
      val _ = Check.expect (Compile.code "[F ? [Z + SZ]: Z]", [A.Cond (A.False, A.Add (A.Zero, A.Succ A.Zero), A.Zero), A.Zero], "compile14")  
      val _ = Check.exn (fn () => Compile.code "Z+Z", "badCompile0")
      val _ = Check.exn (fn () => Compile.code "[Z+Z", "badCompile1")
      val _ = Check.exn (fn () => Compile.code "[ZZ+Z]", "badCompile2")
      val _ = Check.exn (fn () => Compile.code "[T ? F: [F ? SZ :]]", "badCompile3")
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
