structure TypeCheck : sig

(* helper *)
val inLw : (string * Type.typ) * ((string * Type.typ) list) -> bool
val width : Type.typ * Type.typ -> bool
val inLd : (string * Type.typ) * ((string * Type.typ) list) -> (string * Type.typ) option
val getMatches : Type.typ * Type.typ -> (string * Type.typ) list
(* val perm : Type.typ * Type.typ -> bool *)
val typeof_helper : (TypeEnv.env * L23RR.term) -> Type.typ

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv

  fun inLw (target, lst) =
    (case target of
        (target_l, target_t) =>
          (case lst of
              ((l1,ty1)::rest) => 
                if String.compare(target_l, l1) = EQUAL andalso target_t = ty1 then
                  true
                else 
                  inLw(target, rest)
            | _ => false
          )
      (* | _ => raise Fail "inLw: no designated target" *)
    )

  fun width (t1, t2) = 
    (case t2 of
        T.Record((l1,ty1)::t2_rest) =>  
          (case t1 of
              T.Record(t1_lst) =>
                if inLw((l1,ty1), t1_lst) then
                  width(t1, T.Record(t2_rest))
                else
                  false
            | _ => raise Fail "width: should not take in non-record type"
          )
      | T.Record([]) => true
      | _ => raise Fail "width: should not take in non-record type"
    )

  fun inLd ((target_l, target_t), lst) =
    (case lst of
        ((l1,ty1)::rest) => 
          if String.compare(target_l, l1) = EQUAL then
            SOME((l1,ty1))
          else 
            inLd((target_l, target_t), rest)
      | _ => NONE
    )
      (* | _ => raise Fail "inLd: no designated target" *)
  
  fun getMatches (t1, t2) = 
    (case t1 of
        T.Record((t1_l1,t1_ty1)::t1_rest) => 
          (case t2 of
              T.Record((t2_l1,t2_ty1)::t2_rest) => 
                (case inLd((t1_l1,t1_ty1), (t2_l1,t2_ty1)::t2_rest) of
                    SOME(t2_return) => (t2_return::(getMatches(T.Record(t1_rest), t2)))
                  | _ => []
                )
            | _ => []
          )
      | _ => []
    )

  (* fun perm (t1, t2) = raise Fail "todo" *)
		  
  (* fun subty _ = raise Fail "todo: TypeCheck.subty" *)
  fun subty (t1, t2) = 
    (case t1 of
        T.Record((t1_l1,t1_ty1)::t1_rest) => 
          (case t2 of
              T.Record((t2_l1,t2_ty1)::t2_rest) => 
                if width(t1, t2) then 
                  true
                else (*depth*)
                  let 
                    val t2_matches = getMatches(t1, t2)
                    val t1_matches = getMatches(t2, t1)
                    fun depth (t1_lst, t2_lst) =
                      (case t2_lst of
                          [] => 
                            (case t1_lst of
                                [] => true
                              | _ => false
                            )
                        | (_, t2_lst_t)::t2_lst_rest =>
                            (case t1_lst of
                                [] => false
                              | (_,t1_lst_ty1)::t1_lst_rest =>
                                  if subty(t1_lst_ty1,t2_lst_t) then 
                                    depth(t1_lst_rest, t2_lst_rest)
                                  else false
                            )
                      )
                  in
                    (* second call is for length checking. matches capped out by how size of record. if same size then both calls same args *)
                    depth((t1_l1,t1_ty1)::t1_rest, t2_matches) andalso depth(t1_matches, (t2_l1,t2_ty1)::t2_rest)
                  end
            | T.Record([]) => raise Fail "subty: parser error, record type empty"
            | _ => false
          )
      | T.Record([]) => raise Fail "subty: parser error, record type empty"
      | T.Function(t1_ty1, t1_ty2) =>
        (case t2 of
            T.Function(t2_ty1, t2_ty2) =>
              subty(t2_ty1, t1_ty1) andalso subty(t1_ty2, t2_ty2)
          | _ => false
        )
      | T.Int =>
          (case t2 of
              T.Int => true
            | _ => false
          )
      | T.Bool =>
          (case t2 of
              T.Bool => true
            | _ => false
          )
      | T.Unit =>
          (case t2 of
              T.Unit => true
            | _ => false
          )
    )

  (* fun commonSupertype _ = raise Fail "todo: TypeCheck.commonSupertype" *)
  fun commonSupertype (t1, t2) =
    if subty(t1, t2) then 
      SOME t2
    else if subty(t2, t1) then
      SOME t1
    else 
      raise Fail "commonSupertype: no supertype found"

  
  fun typeof_helper (gamma, term) = 
    (case term of
        L.Int(n) => T.Int 
      | L.True => T.Bool
      | L.False => T.Bool 
      | L.Unit => T.Unit
      | L.Var(x) =>   
          (case TypeEnv.lookup(gamma, x) of 
              SOME(tp) => tp
            | _ => raise Fail "typeof_helper, var: lookup fail, term not found"
          )
      | L.Lam(x, tau1, t1) => T.Function(tau1, typeof_helper(gamma, t1))
      | L.App(t1, t2) =>
          (case typeof_helper(gamma, t1) of
              T.Function(tau1, tau2) =>
                (case typeof_helper(gamma, t2) of
                    (* T.Function(_,_) => raise Fail "typeof_helper, app: t2 cannot be of type function" *)
                  tau3 =>
                    if subty(tau3, tau1) then
                      tau2
                    else
                      raise Fail "typeof_helper, app: subty fail"
                )
            | _ => raise Fail "typeof_helper, app: t1 must be of type function"
          )
      | L.Fix(t1) =>
          (case typeof_helper(gamma, t1) of
              T.Function(tau1, tau2) =>
                if tau1 = tau2 then
                  tau1
                else
                  raise Fail "typeof_helper, fix: function must have same input/output type"
            | _ => raise Fail "typeof_helper, fix: term must be of type function"
          )
      | L.Let(x, t1, t2) => 
          let
            val gamma_new = TypeEnv.extend(gamma, x, typeof_helper(gamma, t1))
          in
            typeof_helper(gamma_new, t2)
          end
      | L.Cond(t1, t2, t3) =>
          (case typeof_helper(gamma, t1) of
              T.Bool => 
                (case commonSupertype(typeof_helper(gamma, t2), typeof_helper(gamma, t3)) of
                    SOME(tau4) => tau4
                  | _ => raise Fail "typeof_helper, cond: commonSupertype error, no supertype found"
                )
            | _ => raise Fail "typeof_helper, cond: t1 must type bool"
          )
      | L.Add(t1, t2) => 
          (case typeof_helper(gamma, t1) of
              T.Int =>
                (case typeof_helper(gamma, t2) of
                    T.Int => T.Int
                  | _ => raise Fail "typeof_helper, add: t2 must by type int"
                )
            | _ => raise Fail "typeof_helper, add: t1 must by type int"
          )
      | L.Sub(t1, t2) => 
          (case typeof_helper(gamma, t1) of
              T.Int =>
                (case typeof_helper(gamma, t2) of
                    T.Int => T.Int
                  | _ => raise Fail "typeof_helper, sub: t2 must by type int"
                )
            | _ => raise Fail "typeof_helper, sub: t1 must by type int"
          )
      | L.Mul(t1, t2) => 
          (case typeof_helper(gamma, t1) of
              T.Int =>
                (case typeof_helper(gamma, t2) of
                    T.Int => T.Int
                  | _ => raise Fail "typeof_helper, mul: t2 must by type int"
                )
            | _ => raise Fail "typeof_helper, mul: t1 must by type int"
          )
      | L.Eq(t1, t2) =>
          (case typeof_helper(gamma, t1) of
              T.Int =>
                (case typeof_helper(gamma, t2) of
                  T.Int => T.Bool
                | _ => raise Fail "typeof_helper, eq: t2 must by type int"
                )
            | _ => raise Fail "typeof_helper, eq: t1 must by type int"
          )
      | L.LessThan(t1, t2) =>
          (case typeof_helper(gamma, t1) of
              T.Int =>
                (case typeof_helper(gamma, t2) of
                  T.Int => T.Bool
                | _ => raise Fail "typeof_helper, lessthan: t2 must by type int"
                )
            | _ => raise Fail "typeof_helper, lessthan: t1 must by type int"
          )
      | L.Not(t1) =>
          (case typeof_helper(gamma, t1) of
              T.Bool => T.Bool
            | _ => raise Fail "typeof_helper, not: t1 must be type bool"
          )
      | L.Record(lst) => 
          let
            fun get_rec_type rec_lst = 
              (case rec_lst of
                  ((l1, t1)::rest) => (l1, typeof_helper(gamma, t1))::(get_rec_type(rest))
                | [] => []
                (* | _ => raise Fail "typeof_helper, record: lst must be a (string * Type.typ) list" *)
              )
          in
            T.Record(get_rec_type(lst))
          end
      | L.Select(l, t1) => 
          (case t1 of
              L.Record((l1, f1)::rest) =>
                if l = l1 then
                  typeof_helper(gamma, f1)
                else
                  typeof_helper(gamma, L.Select(l, L.Record(rest)))
            (* | L.Select(_,_) => typeof_helper(gamma, t1) *)

            (* | L.Record([]) => raise Fail "testinggs" *)
            | _ => raise Fail "typeof_helper, select: t1 must be type record"
            (* | _ => typeof_helper(gamma, t1) *)
          )
          (* (case typeof_helper(gamma, t1) of
              T.Record((l1, ty1)::rest) =>
                if l = l1 then
          ) *)
    )

  (* fun typeof _ = raise Fail "todo: TypeCheck.typeof" *)
  fun typeof (term) = typeof_helper(E.empty, term)
	    
end


  (* fun inLd ((t1_l1,t1_ty1)::t1_rest, (t2_l1,t2_ty1)::t2_rest) = 
    case  *)
    (* let
      val inL1 = inLd_helper((t1_l1,t1_ty1), ((t2_l1,t2_ty1)::t2_rest))
      val inL2 = inLd_helper((t2_l1,t2_ty1), ((t1_l1,t1_ty1)::t1_rest))
    in
      if inL1 = 
    end *)

  (* fun getMatches (t1, t2) = 
    (case t1 of
        T.Record([]) =>
          (case t2 of
              T.Record([]) => true
            | _ => false
          )
      | T.Record((t1_l1, t1_ty1)::t1_rest) =>
          (case t2 of
              T.Record((t2_l1,t2_ty1)::t2_rest) =>
                (case inLd((t1_l1,t1_ty1), ((t2_l1,t2_ty1)::t2_rest)) of
                    SOME(t2_return) =>

                )
                (* if inLd ((t1_l1, t1_ty1), t2_lst) then
                  getMatches(T.Record(t1_rest), t2)
                else
                  false *)
            | _ => raise Fail "getMatches: should not take in non-record type"
          )
      | _ => raise Fail "getMatches: should not take in non-record type"
    ) *)


    (* (case t1 of
        T.Record((t1_l1,t1_ty1)::t1_rest) =>  
          (case t2 of
              T.Record((t2_l1,t2_ty1)::t2_rest) =>
                if t1_ty1 = t2_ty1 andalso t1_l1 = t2_l1 then
                  width(T.Record(t1_rest), T.Record(t2_rest))
                else
                  width(T.Record(t1, T.Record(t2_rest)))
            | T.Record([]) => true
            | _ => raise Fail "width: should not take in non-record type" 
          )
      | T.Record([]) => 
          (case t2 of
              T.Record((_,_)::rest) => false
            | T.Record([]) => true
            | _ => raise Fail "width: should not take in non-record type"
          )
      | T.Function(t1_ty1, t1_ty2) =>
      | _ => raise Fail "width: should not take in non-record type"
    ) *)


                      (* (case inLd((t1_l1,t1_ty1), ((t2_l1,t2_ty1)::t2_rest)) of 
                        (* inLd((t2_l1,t2_ty1), ((t1_l1,t1_ty1)::t1_rest)) of *)
                      SOME(t2_return) => 
                        if subty(t1_ty1, t2_return) then
                          (case t1_rest of
                              [] => 
                                (case t2_rest of
                                    [] => true
                                  | _ => false
                                )
                            | (t1_l2,t1_ty2)::t1_rest' => 
                                inLd((t1_l2,t1_ty2), ((t2_l1,t2_ty1)::t2_rest))
                          )
                        else false
                        (* (case inLd((t2_l1,t2_ty1), ((t1_l1,t1_ty1)::t1_rest)) of
                            SOME(t1_return) =>
                              subty(t1_ty1, t2_return) andalso subty(t2_ty1, t1_return)
                          | _ => false
                        ) *)
                    | _ => false
                  ) *)


                 (* if width(t1, t2) then true
                else if isLd((t1_l1,t1_ty1), ((t2_l1, t2_ty1)::t2_rest))
                (* else if perm(t1, t2) then true *)
                else false *)     

                    (* (case t2_matches of
                        [] => false (* bc t2 never empty in this branch *)
                      | _ => 
                        (case t1_matches of
                            [] => false
                          | _ => depth((t1_l1,t1_ty1)::t1_rest, t2_matches) andalso depth(t1_matches, (t2_l1,t2_ty1)::t2_rest)
                        )
                    ) *)                