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

  (* fun isV term = case term of 
      D.Zero => true 
      | D.Succ(t) => isV t 
      | D.Pair(t1, t2) => isV t1 andalso isV t2 
      | _ => false  *)
		 
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
      
      (* | D.First(t1) =>
        (case t1 of
            D.Pair(n1, n2) =>
              if isV(t1) then SOME(n1)
              else
                (case step t1 of
                    SOME(D.Pair(n1', n2')) => SOME(n1')
                  | _ => NONE
                )
          | _ => raise Fail "first: typechecker not working"
        ) *)

      (* | D.First(t1) =>
        (case t1 of
            D.Pair(n1, n2) =>
              if isV(t1) then SOME(n1)
              else
                (case step t1 of
                    SOME(D.Pair(n1', n2')) => SOME(n1')
                  | _ => NONE
                )
          | _ => raise Fail "first: typechecker not working"
        )
      
      | D.Second(t1) =>
        (case t1 of
            D.Pair(n1, n2) =>
              if isV(t1) then SOME(n2)
              else
                (case step t1 of
                    SOME(D.Pair(n1', n2')) => SOME(n2')
                  | _ => NONE
                )
          | _ => raise Fail "second: typechecker not working"
        ) *)

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

        (* | D.Second(t) => 
                if isV t 
                    then (case t of
                        D.Pair(a, b) => SOME(b)
                        | _ => NONE 
                    )
                else (case step t of
                       SOME(a) => SOME(D.Second(a))
                       | _ => NONE 
                )  *)

      (* | _ => raise Fail "Eval.step: invalid desugared input type" *)
    )

  (* fun step term = 
      if isV term 
          then NONE 
      else 
          case term of 
              D.Succ(t) => (case step t of
                              SOME(a) => SOME(D.Succ(a))
                              | _ => NONE 
                          )
                          
              | D.Add(t1, t2) => 
              if isV t1 
                then (case t1 of 
                  D.Zero => SOME(t2) 
                  (* this hinges on t2 being a v, right? but theres no check on that. or can t be anything inc not a v? *)
                  | D.Succ(a) => if isV a = true 
                                    then SOME(D.Add(a, D.Succ(t2)))
                                else NONE
                  | _ => NONE
                ) 
              else (case step t1 of
                SOME(a) => SOME(D.Add(a, t2))
                | _ => NONE
              )

              | D.Subtract(t1, t2) => 
                if isV t1 
                    then (case step t2 of
                      SOME(a) => SOME(D.Subtract(t1, a))
                      | NONE => if t2 = D.Zero 
                                    then SOME(t1)
                                (* check that t2 is not a pair right? nah we dont check *)
                                else if t1 = D.Zero andalso isV t2 
                                    then SOME(D.Zero)
                                else (case t1 of 
                                      D.Succ(i) => (case t2 of
                                          (* do we need this if? there cant be a succ of a non-V right *)
                                          D.Succ(j) => if isV i andalso isV j
                                                            then SOME(D.Subtract(i, j))
                                                        else NONE 
                                          | _ => NONE
                                          )
                                      | _ => NONE 
                                      )
                      )
                else (case step t1 of 
                  SOME(a) => SOME(D.Subtract(a, t2))
                  | _ => NONE (* does this mean it's stuck? ^ *)
                ) 

              | D.Less(t1, t2) => 
                if isV t1 
                    then (case t1 of 
                          D.Zero => if isV t2
                                        (* this is boolean true right *)
                                        then (case t2 of
                                            D.Zero => SOME(D.Zero)
                                            | D.Succ(a) => (if isV a 
                                                                then SOME(D.Succ(D.Zero))
                                                            else NONE 
                                                            )
                                            (* if its a pair or smth it doesnt work *)
                                            | _ => NONE                  
                                        )
                                    else (case step t2 of
                                        SOME(a) => SOME(D.Less(t1, a))
                                        | _ => NONE
                                    )
                          | D.Succ(a) => if isV t2 
                                            then (case t2 of
                                                D.Zero => SOME(D.Zero)
                                                | D.Succ(b) => SOME(D.Less(a, b))
                                                | _ => NONE 
                                              )
                                          else (case step t2 of
                                              SOME(a) => SOME(D.Less(t1, a))
                                              | _ => NONE 
                                          )
                          (* what if t1 is a pair? adjust for types *)
                          | _ => NONE 
                      )
                else (case step t1 of 
                    SOME(a) => SOME(D.Less(a, t2))
                    | _ => NONE 
                )

              | D.Eq(t1, t2) => 
                if isV t1 
                    then (case t1 of 
                          D.Zero => if isV t2 
                                        then (case t2 of 
                                          D.Zero => SOME(D.Succ(D.Zero))
                                          | D.Succ(a) => if isV a 
                                                            then SOME(D.Zero)
                                                        else NONE 
                                          | _ => NONE               
                                        )
                                    else (case step t2 of
                                          SOME(a) => SOME(D.Eq(t1, a))
                                          | _ => NONE 
                                        )
                          | D.Succ(a) => if isV t2 
                                            then (case t2 of
                                                 D.Zero => SOME(D.Zero)
                                                 | D.Succ(b) => if isV b 
                                                                    then SOME(D.Eq(a, b))
                                                                else NONE 
                                                 | _ => NONE 
                                            )
                                          else (case step t2 of
                                                SOME(a) => SOME(D.Eq(t1, a))
                                                | _ => NONE 
                                          )
                          | D.Pair(a, b) => (case t2 of
                                            D.Pair(c, d) => SOME(D.Cond(D.Eq(a, c), D.Eq(b, d), D.Zero))
                                            | _ => NONE 
                          )
                    )
                else (case step t1 of 
                      SOME(a) => SOME(D.Eq(a, t2))
                )

              | D.Cond(t1, t2, t3) => 
                if isV t1 
                    then (case t1 of 
                          D.Succ(D.Zero) => SOME(t2)
                          | D.Zero => SOME(t3)
                          (* if its a pair i guess *)
                          | _ => NONE
                    )
                else (case step t1 of
                      SOME(a) => SOME(D.Cond(a, t2, t3))
                      | _ => NONE
                )

              | D.Pair(t1, t2) => 
                if isV t1
                    then (case step t2 of
                           SOME(a) => SOME(D.Pair(t1, a))
                           | _ => NONE
                        )
                else 
                    (case step t1 of
                      SOME(a) => SOME(D.Pair(a, t2))
                      | _ => NONE 
                  )
              
              | D.First(t) => 
                if isV t 
                    then (case t of
                        D.Pair(a, b) => SOME(a)
                        | _ => NONE 
                    )
                else (case step t of
                       SOME(a) => SOME(D.First(a))
                       | _ => NONE 
                )

                | D.Second(t) => 
                if isV t 
                    then (case t of
                        D.Pair(a, b) => SOME(b)
                        | _ => NONE 
                    )
                else (case step t of
                       SOME(a) => SOME(D.Second(a))
                       | _ => NONE 
                ) *)
				    
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

    (* if isV(List.last(eval(term))) 
    then (Value last)
    else (Stuck last) *)

    (* (case List.last(eval term) of
        last => 
          if isV(last) 
          then (Value last)
          else (Stuck last)
      | _ => raise Fail "empty eval list"
    ) *)


    (* if isV(x)
    then (Value(x))
    else (Stuck(x)) *)
    (* (case last of
        x => 
          if isV(x)
          then (Value(x))
          else (Stuck(x))
      | _ => raise Fail "should not be empty input"
    ) *)
    (* let
      fun lastElement(lst) =
        (case lst of
            [] => NONE
          | [x] => SOME(x)
          | (_::xs) => lastElement(xs)
        )
    in
      (case lastElement steps of
          SOME(x) => 
            if isV(x)
            then (Value(x))
            else (Stuck(x))
        | _ => raise Fail "should not be empty input"
      )
    end *)

end
