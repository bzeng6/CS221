structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
    let
      val _ = Check.expect (Scan.scan "12", [T.Nat 12], "scan12")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
      val _ = Check.expect (Scan.scan " 123456789", [T.Nat 123456789], "scan1")
      val _ = Check.expect (Scan.scan "112 12", [T.Nat 112, T.Nat 12], "scan2")
      val _ = Check.expect (Scan.scan "0", [T.Nat 0], "scan0")
      (* val _ = Check.expect (Scan.scan "34 ") *)
      (* val _ = Check.expect (Scan.scan "[ [[T || F] && T] && T ]", [T]) *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun typ () =
    let
      val _ = Check.expect (TypeCheck.typeof (S.Nat 12), Type.Nat, "type12") 
      val _ = Check.expect (TypeCheck.typeof (S.Cond(S.True, (S.Nat 8), (S.Nat 88))), Type.Nat, "type1")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond(S.True, (S.Nat 8), (S.False))), "badtype00")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond((S.Nat 3), (S.Nat 8), (S.False))), "badtype02")
      val _ = Check.expect (TypeCheck.typeof (S.Not(S.True)), Type.Bool, "type2")
      val _ = Check.expect (TypeCheck.typeof (S.Pair(S.True, (S.Add((S.Nat 5), (S.Nat 10))))), Type.Product(Type.Bool, Type.Nat), "type 3")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Pair(S.True, (S.Add((S.Nat 5), S.False)))), "badtype01")
      val _ = Check.expect (TypeCheck.typeof (S.And(S.And(S.True, S.True), S.True)), Type.Bool, "type4")
      (* val _ = Check.exn (fn () => TypeCheck.typeof (S.Nat S.Pair(S.True, S.False)), "badtype02") *)
    in
      TextIO.print "type tests done\n"
    end

  fun desugar () =
    let
      val desugar = Desugar.desugar
      val _ = Check.expect (desugar (S.Nat 0), D.Zero, "desugar0")
      val _ = Check.expect (desugar (S.Nat 1), D.Succ(D.Zero), "desugar1")
      val _ = Check.expect (desugar (S.False), D.Zero, "desugar2")
      val _ = Check.expect (desugar (S.True), D.Succ(D.Zero), "desugar3")
      val _ = Check.expect (desugar (S.Unit), D.Zero, "desugar4")
      val _ = Check.expect (desugar (S.Less(S.Nat(0), S.Nat(1))), D.Less(D.Zero, D.Succ(D.Zero)), "desugar5")
      val _ = Check.expect (desugar (S.And(S.True, S.True)), D.Cond(D.Eq(D.Succ(D.Zero), D.Succ(D.Zero)), D.Eq(D.Succ(D.Zero), D.Succ(D.Zero)), D.Zero), "desugar6")
    in
      TextIO.print "desugar tests done\n"
    end
			            
  fun eval () =
    let
      val _ = Check.expect (Eval.result D.Zero, Eval.Value D.Zero, "eval0")
      (* write more eval tests here *)
      (* val _ = Check.expect (Eval.result (D.First (D.Pair(D.Zero, D.Add (D.Succ(D.Zero))))), Eval.Stuck (D.First (D.Succ(D.Zero))), "eval1#[1+0]") *)
      val _ = Check.expect (Eval.result (D.Second (D.Pair(D.Zero, D.Zero))), Eval.Value(D.Zero), "eval1")
      val _ = Check.expect (Eval.result (D.First (D.Pair(D.Succ(D.Zero), D.Zero))), Eval.Value(D.Succ(D.Zero)), "eval2")
      val _ = Check.expect (Eval.result (D.First (D.Pair(D.Add((D.Nat 12), D.Zero), D.Zero))), Eval.Value((D.Nat 12)), "eval3")
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      fun value typ program result =
	Check.expect (Compile.code program, (E.Value result, typ), "compile"^program)
      val natval = value Type.Nat
      val boolval = value Type.Bool
      val _ = natval "0" D.Zero 
      (* write more compile tests here *)
      val _ = natval "[1+1]" (D.Succ (D.Succ D.Zero))
      val _ = natval "[2-1]" (D.Succ D.Zero)
      val _ = boolval "[8>8]" D.Zero 
      val _ = boolval "[8 >= 8]" (D.Succ D.Zero)
      val _ = boolval "[8 < 8]" D.Zero 
      val _ = boolval "[8 <= 8]" (D.Succ D.Zero)
      val _ = natval "[ [[T && T] && T] ? [[10 + 12] - 20]:  [10 - 1#(9, 8)]]" (D.Succ((D.Succ D.Zero)))
      val _ = boolval "[[T && T] && T]" (D.Succ D.Zero)
      val _ = natval "[ [ [[T || F] && T] && T ] ? [[10 + 12] - 20] :  [10 - 1#(9, 8)]]" (D.Succ((D.Succ D.Zero)))
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = typ ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end