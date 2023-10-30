structure Test : sig

  val all : unit -> unit
  val ur : unit -> unit
	    
end = struct

  fun println s = TextIO.print (s ^ "\n")

  (* some renamings for convenience... *)
  val lam = ULC.Lam
  val v   = ULC.Var

  fun ur () = 
    let
      val _ = Check.expect (Unroll.unroll 
                (Sweetl.Prog([("ab", Sweetl.Var("aabb")), ("x", Sweetl.Var("xx"))], 
                              Sweetl.Abbr("x"))), 
                Sweetl.Var("xx"), 
                "test0")
      val _ = Check.expect (Unroll.unroll 
                (Sweetl.Prog ([("ab",Sweetl.Lam ("a", Sweetl.Var "b")),("cd", Sweetl.Lam ("c", Sweetl.Var "d"))],
                              Sweetl.App (Sweetl.Abbr "ab", Sweetl.Abbr "cd"))),
                Sweetl.App (Sweetl.Lam ("a", Sweetl.Var "b"), Sweetl.Lam ("c", Sweetl.Var "d")), 
                "test1")
    in
      println "== unroll tests complete"
    end
			       
  fun all () =
    let
      val _ = Check.expect (Compile.cbv "([x x] [y y])",
			    lam ("y", v "y"),
			    "test0")
      val _ = Check.expect (Compile.cbv "(&x &y)",
			    lam ("y", v "y"),
			    "test1")
      val _ = Check.expect (Compile.cbv ":idx=&x; (:idx &y)",
			    lam ("y", v "y"),
			    "test2")
      (* tests here *)                       
    in
      println "== tests complete"
    end

end
