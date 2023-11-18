structure Eval : sig

  (* helper *)
  val isV : L23RR.term -> bool

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun isV term = 
    (case term of 
        L.Int(n) => true 
      | L.True => true 
      | L.False => true 
      | L.Unit => true 
      | L.Lam(x, tau1, t1) => true
      | L.Record(lst) =>
          (case lst of
              (l1, t1)::rest => 
                if isV(t1) then
                  isV(L.Record(rest))
                else 
                  false 
            | [] => true
          )                  
      | _ => false 
    )

  (* fun eval _ = raise Fail "todo: Eval.eval" *)
  fun eval term = 
    (case term of
        L.Int(n) => L.Int(n)
      | L.True => L.True
      | L.False => L.False
      | L.Unit => L.Unit
      | L.Lam(x, tau1, t1) => L.Lam(x, tau1, t1)
      | L.App(t1, t2) => 
          (case eval(t1) of
              L.Lam(x, tau1, t11) =>
                (case eval(t2) of
                    v2 => 
                      (case eval(Subst.subst(x, v2, t11)) of
                          v3 => 
                            if isV(v2) andalso isV(v3) then 
                              v3
                            else
                              raise Fail "eval, app: evaluation stuck"
                      )
                )
            | _ => raise Fail "eval, app: first arg should eval to lam"
          )
      | L.Fix(t1) =>
          (case eval(t1) of
              L.Lam(f, tau1, t11) =>
                (case eval(Subst.subst(f, L.Fix(L.Lam(f, tau1, t11)), t11)) of
                    v1 =>
                      if isV(v1) then
                        v1
                      else
                        raise Fail "eval, fix: evaluation stuck"
                )
            | _ => raise Fail "eval, fix: first arg should eval to lam"
          )
      | L.Let(x, t1, t2) =>
          (case eval(t1) of
              v1 =>
                (case eval(Subst.subst(x, v1, t2)) of
                    v2 => 
                      if isV(v1) andalso isV(v2) then
                        v2
                      else 
                        raise Fail "eval, let: evaluation stuck"
                )
          )
      | L.Cond(t1, t2, t3) =>
          (case eval(t1) of
              L.True =>
                (case eval(t2) of
                    v2 =>
                      if isV(v2) then
                        v2
                      else
                        raise Fail "eval, cond-T: arg 2 evaluation stuck"
                )
            | L.False =>
                (case eval(t3) of
                    v3 =>
                      if isV(v3) then
                        v3
                      else
                        raise Fail "eval, cond-F: arg 3 evaluation stuck"
                )
            | _ => raise Fail "eval, cond: typecheck error, first arg should eval to bool"
          )
      | L.Add(t1, t2) =>
          (case eval(t1) of
              L.Int(n1) =>
                (case eval(t2) of
                    L.Int(n2) => L.Int(n1+n2)
                  | _ => raise Fail "eval, add: typecheck error, second arg should eval to int"
                )
            | _ => raise Fail "eval, add: typecheck error, first arg should eval to int"
          )
      | L.Sub(t1, t2) =>
          (case eval(t1) of
              L.Int(n1) =>
                (case eval(t2) of
                    L.Int(n2) => L.Int(n1-n2)
                  | _ => raise Fail "eval, sub: typecheck error, second arg should eval to int"
                )
            | _ => raise Fail "eval, sub: typecheck error, first arg should eval to int"
          )
      | L.Mul(t1, t2) =>
          (case eval(t1) of
              L.Int(n1) =>
                (case eval(t2) of
                    L.Int(n2) => L.Int(n1*n2)
                  | _ => raise Fail "eval, mul: typecheck error, second arg should eval to int"
                )
            | _ => raise Fail "eval, mul: typecheck error, first arg should eval to int"
          )
      | L.Eq(t1, t2) =>
          (case eval(t1) of
              L.Int(n1) =>
                (case eval(t2) of
                    L.Int(n2) => 
                      if n1 = n2 then
                        L.True
                      else
                        L.False
                  | _ => raise Fail "eval, eq: typecheck error, second arg should eval to int"
                )
            | _ => raise Fail "eval, eq: typecheck error, first arg should eval to int"
          )
      | L.LessThan(t1, t2) =>
          (case eval(t1) of
              L.Int(n1) =>
                (case eval(t2) of
                    L.Int(n2) => 
                      if n1 < n2 then
                        L.True
                      else
                        L.False
                  | _ => raise Fail "eval, lessthan: typecheck error, second arg should eval to int"
                )
            | _ => raise Fail "eval, lessthan: typecheck error, first arg should eval to int"
          )
      | L.Not(t1) =>
          (case eval(t1) of
              L.True => L.False
            | L.False => L.True
            | _ => raise Fail "eval, not: typecheck error, arg should eval to bool"
          )
      | L.Record(lst) =>
          let
            fun evalrec rlist =
              (case rlist of
                  (l1, t1)::rest =>
                      (case eval(t1) of
                          v1 =>
                            if isV(v1) then
                              (l1, v1)::(evalrec(rest))
                            else
                              raise Fail "eval, record: field evaluation stuck"  
                      )
                | _ => []
              )
          in
            L.Record(evalrec(lst))
          end
      | L.Select(l_target, t1) =>
          (case eval(t1) of
              L.Record(lst) =>
                let
                  fun inL reclist =
                    (case reclist of
                        (l1, v1)::rest =>
                          if l_target = l1 then
                            if isV(v1) then
                              v1
                            else
                              raise Fail "eval, select: field with given label not value"
                          else
                            inL(rest)
                      | _ => raise Fail "eval, select: label not found in record"
                    )
                in
                  inL(lst)
                end
            | _ => raise Fail "eval, select: term should eval to record"
          )
      | _ => raise Fail "eval: invalid input"
    )
		 
end
