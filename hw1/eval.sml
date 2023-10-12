structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  (* fun isNV _ = raise Fail "todo" *)
  fun isNV tok = 
    (case tok of
        A.Zero => true
      | A.Succ(n) => isNV(n)
      | _ => false
    )

  (* fun isV _ = raise Fail "todo" *)
  fun isV tok = 
    (case tok of
        A.True => true
      | A.False => true
      | _ => isNV(tok)
    )
		 
  (* fun step _ = raise Fail "todo" *)
  fun step term =
    (case term of 
        A.Zero => NONE
      | A.True => NONE
      | A.False => NONE
      
      | A.Succ(n) =>
          (case step(n) of
              SOME(n') => SOME(A.Succ(n'))
            | _ => NONE
          )

      | A.Pred(n) =>
          (case n of 
              A.Zero => SOME(A.Zero)
            | A.Succ(n') => SOME(n')
            | n' => 
              (case step(n') of
                  SOME(n'') => SOME(A.Pred(n'')) (* ?? *)
                | _ => NONE
              )
          )

      | A.Add(t1, t2) =>
        if isNV(t1)
        then 
          (case t1 of
              A.Zero => SOME(t2)
            | A.Succ(t1') => SOME(A.Add(t1', A.Succ(t2))) (* ?? *)
            | _ => raise Fail "add: isNV check not working"
          )
        else
          (case step(t1) of
              SOME(t1') => SOME(A.Add(t1', t2))
            | _ => NONE
          )

      | A.Subtract(t1, t2) =>
        if isV(t1) then
          if isNV(t1) then
            (case t1 of
                A.Zero => SOME(A.Zero)
              | A.Succ(_) =>
                (case t2 of
                    A.Zero => SOME(t1)
                  | A.Succ(_) => SOME(A.Subtract(t1, t2))
                  | _ =>
                    (case step t2 of
                        SOME(t2') => SOME(A.Subtract(t1, t2'))
                      | _ => NONE
                    )
                )
              | _ => raise Fail ("subtract: isNV failed")
            )
          else
            (case step t2 of
                SOME(t2') => SOME(A.Subtract(t1, t2'))
              | _ => NONE
            )
        else
          (case step t1 of
              SOME(t1') => SOME(A.Subtract(t1', t2))
            | _ => NONE
          )

      | A.Less(t1, t2) =>
        if isV(t1) then
          if isNV(t1) then
            (case t1 of
                A.Zero =>
                  (case t2 of
                      A.Zero => SOME(A.False)
                    | A.Succ(_) => SOME(A.True)
                    | _ =>
                      (case step t2 of
                          SOME(t2') => SOME(A.Less(t1, t2'))
                        | _ => NONE
                      )
                  )
              | A.Succ(t1') => 
                  (case t2 of
                      A.Zero => SOME(A.False)
                    | A.Succ(t2') => SOME(A.Less(t1', t2'))
                    | _ => 
                      (case step t2 of
                          SOME(t2') => SOME(A.Less(t1, t2'))
                        | _ => NONE
                      )
                  )
              | _ => raise Fail "less: isNV check not working"
            )
          else 
            (case step t2 of
                SOME(t2') => SOME(A.Less(t1, t2'))
              | _ => NONE
            )
        else
          (case step t1 of
              SOME(t1') => SOME(A.Less(t1', t2))
            | _ => NONE
          )

      | A.Greater(t1, t2) =>
        if isV(t1) then
          if isNV(t1) then
            (case t1 of
                A.Zero => 
                  if isNV(t2) then
                    SOME(A.False)
                  else
                    (case step t2 of
                        SOME(t2') => SOME(A.Greater(t1, t2'))
                      | _ => NONE
                    )
              | A.Succ(t1') =>
                (case t2 of
                    A.Zero => SOME(A.True)
                  | A.Succ(t2') => SOME(A.Greater(t1', t2'))
                  | _ =>
                    (case step t2 of
                        SOME(t2') => SOME(A.Greater(t1, t2'))
                      | _ => NONE
                    )
                )
              | _ => raise Fail "greater: isNV check not working"
            )
          else
            (case step t2 of
                SOME(t2') => SOME(A.Greater(t1, t2'))
              | _ => NONE
            )
        else
          (case step t1 of
              SOME(t1') => SOME(A.Greater(t1', t2))
            | _ => NONE
          )
      
      | A.And(t1, t2) =>
        (case t1 of 
            A.True => SOME(t2)
          | A.False => SOME(A.False)
          | _ =>
            (case step(t1) of
                SOME(t1') => SOME(A.And(t1', t2))
              | _ => NONE
            )
        )
      
      | A.Or(t1, t2) =>
        (case t1 of 
            A.True => SOME(A.True)
          | A.False => SOME(t2)
          | _ =>
            (case step(t1) of
                SOME(t1') => SOME(A.Or(t1', t2))
              | _ => NONE
            )
        )
      
      | A.Cond(t1, t2, t3) =>
          (case t1 of
              A.True => SOME(t2)
            | A.False => SOME(t3)
            | _ => 
              (case step(t1) of
                  SOME(t1') => SOME(A.Cond(t1', t2, t3))
                | _ => NONE
              )
          )

      (* | _ => raise Fail "invalid term" *)

    )

  (* fun eval _ = raise Fail "todo" *)
  fun eval terms = 
    (case step terms of
        NONE => [terms]
      | SOME(t) => (terms :: eval(t))
      (* | _ => raise Fail "eval error" *)
    )
	 
end
