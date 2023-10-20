structure Eval : sig

  val isV  : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared

  (* helper *)
  fun isNV n =
    (case n of
        D.Zero => true
      | D.Succ(m) => isNV(m)
      | _ => false
    )

  (* fun isV _ = raise Fail "todo" *)
  fun isV n = 
    (case n of
        D.Pair(t1, t2) =>
          if isV(t1) andalso isV(t2)
          then true
          else false
      | _ => isNV(n)
    )
		 
  (* fun step _ = raise Fail "todo" *)
  fun step term =
    (case term of
        D.Zero => NONE

      (* | D.Succ(_) => NONE *)
      | D.Succ(n) =>
        (case step(n) of
            SOME(n') => SOME(D.Succ(n'))
          | _ => NONE
        )

      | D.Add(t1, t2) =>
        if isV(t1) then 
          (case t1 of
              D.Zero => SOME(t2)
            | D.Succ(t1') => SOME(D.Add(t1', D.Succ(t2))) 
            | D.Pair(_,_) => NONE
            | _ => raise Fail "add: isV check not working"
          )
        else
          (case step(t1) of
              SOME(t1') => SOME(D.Add(t1', t2))
            | _ => NONE
          )
      
      | D.Subtract(t1, t2) =>
        if isV(t1) then
          (case t1 of
              D.Zero => SOME(D.Zero)
            | D.Succ(v1) => 
              (case t2 of
                  D.Zero => SOME(t1)
                | D.Succ(v2) => SOME(D.Subtract(v1, v2))
                | _ =>
                  (case step t2 of
                      SOME(t2') => SOME(D.Subtract(t1, t2'))
                    | _ => NONE
                  )
              )
            | D.Pair(_,_) =>
              (case t2 of
                  D.Zero => SOME(t1)
                | _ =>
                  (case step t2 of
                      SOME(t2') => SOME(D.Subtract(t1, t2'))
                    | _ => NONE
                  )
              )
            | _ => raise Fail "subtract: isV check not working"
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.Subtract(t1', t2))
            | _ => NONE
          )
      
      | D.Less(t1, t2) =>
        if isV(t1) then
          (case t1 of
              D.Zero => 
                (case t2 of
                    D.Zero => SOME(D.Zero)
                  | D.Succ(_) => SOME(D.Succ(D.Zero))
                  | _ => 
                    (case step t2 of
                        SOME(t2') => SOME(D.Less(t1, t2'))
                      | _ => NONE
                    )
                )
            | D.Succ(v1) =>
              (case t2 of
                  D.Zero => SOME(D.Zero)
                | D.Succ(v2) => SOME(D.Less(v1, v2))
                | _ =>
                  (case step t2 of
                      SOME(t2') => SOME(D.Less(t1, t2'))
                    | _ => NONE
                  )
              )
            | D.Pair(_,_) =>
              (case t2 of
                  D.Zero => SOME(D.Zero)
                | _ => 
                  (case step t2 of
                      SOME(t2') => SOME(D.Less(t1, t2'))
                    | _ => NONE
                  )
              )
            | _ => raise Fail "less: isV check not working"
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.Less(t1', t2))
            | _ => NONE
          )

      | D.Eq(t1, t2) =>
        if isV(t1) then
          (case t1 of
              D.Zero =>
                (case t2 of
                    D.Zero => SOME(D.Succ(D.Zero))
                  | D.Succ(D.Zero) => SOME(D.Zero)
                  | _ => 
                    (case step t2 of
                        SOME(t2') => SOME(D.Eq(t1, t2'))
                      | _ => NONE
                    )
                )
            | D.Succ(v1) =>
              (case t2 of
                  D.Zero => SOME(D.Zero)
                | D.Succ(v2) => SOME(D.Eq(v1, v2))
                | _ =>
                  (case step t2 of
                      SOME(t2') => SOME(D.Eq(t1, t2'))
                    | _ => NONE
                  )
              )
            | D.Pair(n1, n2) =>
              if isV(t1) then
                (case t2 of
                    D.Pair(n3, n4) =>
                        if isV(t2) then
                          SOME(D.Cond(D.Eq(n1, n3), D.Eq(n2, n4), D.Zero))
                        else
                          (case step t2 of
                              SOME(t2') => SOME(D.Eq(t1, t2'))
                            | _ => NONE
                          )
                  | _ =>
                    (case step t2 of
                        SOME(t2') => SOME(D.Eq(t1, t2'))
                      | _ => NONE
                    )
                )
              else raise Fail "pair: isV check not working"
            | _ => raise Fail "pair: isV check not working"
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.Eq(t1', t2))
            | _ => NONE
          )
      
      | D.Cond(t1, t2, t3) =>
        (case t1 of
            D.Zero => SOME(t3)
          | D.Succ(D.Zero) => SOME(t2)
          | _ =>
            (case step t1 of
                SOME(t1') => SOME(D.Cond(t1', t2, t3))
              | _ => NONE
            )
        )

      | D.Pair(t1, t2) =>
        if isV(t1)
        then 
          (case step t2 of
              SOME(t2') => SOME(D.Pair(t1, t2'))
            | _ => NONE
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.Pair(t1', t2))
            | _ => NONE
          )

      | D.First(t1) =>
        if isV(t1) then
          (case t1 of
              D.Pair(n1, n2) => SOME(n1)
            | _ => NONE
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.First(t1'))
            | _ => NONE
          )
      
      | D.Second(t1) =>
        if isV(t1) then
          (case t1 of
              D.Pair(n1, n2) => SOME(n2)
            | _ => NONE
          )
        else
          (case step t1 of
              SOME(t1') => SOME(D.Second(t1'))
            | _ => NONE
          )
          (* | _ => raise Fail "second: typechecker not working" *)
				    
  fun eval t =
    let
      fun lp t =
        (case step t
          of SOME t' => t :: lp t'
            | NONE => [t]
        )
    in
      lp t
    end		    

  (* fun result _ = raise Fail "todo" *)
  fun result term =
    let
      val last = List.last(eval term)
    in
      if isV(last) 
      then (Value last)
      else (Stuck last)
    end

end
