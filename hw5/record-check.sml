structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  (* helper *)
  val checkT : Type.typ -> Type.typ

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  structure T = Type

  fun checkT tp = 
    (case tp of
        T.Record([]) => raise Fail "not a valid record type, empty"
      | T.Record((l1, ty1)::[]) =>
          let 
            val check_inner = checkT(ty1)
          in 
            if check_inner = ty1 then 
              tp
            else 
              raise Fail "checkT output not same as input 3" 
          end
      | T.Record((l1, ty1)::(l2, ty2)::rest) => 
          if l1 = l2 then
            raise Fail "not a valid record type, repeated labels"              
          else
            (case checkT(T.Record((l1,ty1)::rest)) of
                T.Record((l1,ty1)::rest) => 
                  (case checkT(T.Record((l2,ty2)::rest)) of
                      T.Record((l2,ty2)::rest) => tp
                    | _ => raise Fail "checkT output not same as input 1"
                  )
              | _ => raise Fail "checkT output not same as input 2"
            )
      | T.Function(ty1, ty2) => T.Function(checkT(ty1), checkT(ty2))
      | _ => tp
    )


  (* fun check _ = raise Fail "todo: RecordCheck.check" *)
  fun check term = 
    (case term of
        L.Record([]) => raise Fail "not a valid record, empty"
      | L.Record((l1, t1)::[]) =>
          let 
            val check_inner = check(t1)
          in 
            if check_inner = t1 then 
              term
            else 
              raise Fail "check output not same as input 3" 
          end
          (* (case check(t1) of
              t1 => term
            (* | _ => raise Fail "fail should have been raised earlier" *)
          ) *)
      | L.Record((l1, t1)::(l2, t2)::rest) => 
          if l1 = l2 then
            raise Fail "not a valid record, repeated labels"              
          else
            (case check(L.Record((l1,t1)::rest)) of
                L.Record((l1,t1)::rest) => 
                  (case check(L.Record((l2,t2)::rest)) of
                      L.Record((l2,t2)::rest) => term
                    | _ => raise Fail "check output not same as input 1"
                  )
              | _ => raise Fail "check output not same as input 2"
            )
      | L.Lam(s1, ty1, t1) => L.Lam(s1, checkT(ty1), check(t1))
      | L.App(t1, t2) => L.App(check(t1), check(t2))
      | L.Fix(t1) => L.Fix(check(t1))
      | L.Let(s1, t1, t2) => L.Let(s1, check(t1), check(t2))
      | L.Cond(t1, t2, t3) => L.Cond(check(t1), check(t2), check(t3))
      | L.Add(t1, t2) => L.Add(check(t1), check(t2))
      | L.Sub(t1, t2) => L.Sub(check(t1), check(t2))
      | L.Mul(t1, t2) => L.Mul(check(t1), check(t2))
      | L.Eq(t1, t2) => L.Eq(check(t1), check(t2))
      | L.LessThan(t1, t2) => L.LessThan(check(t1), check(t2))
      | L.Not(t1) => L.Not(check(t1))
      | L.Select(s1, t1) => L.Select(s1, check(t1))                
      | _ => term
    )

end


(* | L.Record((l1, t1)::rest) =>
      (case rest of
          ((l2, t2)::rest') =>
            (case rest' of
                ((l3, t3)::rest'') =>
                  if l1 != l2 then
                    (case check(L.Record((l1, t1)::rest')) of
                        L.Record((l1, t1)::rest') => c
                    )
                    
                  else
                    raise Fail "not a valid record, repeated labels"
              | _ => 
                (case check(t2) of
                    t2 => term
                )
            )
            
        | NONE => 
          (case check(t1) of
              t1 => check(L.Record(rest))
            | _ => raise Fail "not a valid record, repeated labels"
          )
        
        
      ) *)
