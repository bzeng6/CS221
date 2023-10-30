structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = []  (* <== Change this to something consistent with the new set type. *)

  (* fun mem _ = raise Fail "todo: VarSet.mem" *)
  fun mem (term, s) = 
    (case s of
        (t::ts) => 
          if term = t then
            true
          else
            mem(term, ts)
      | _ => false
    )

  (* fun ins _ = raise Fail "todo: VarSet.ins" *)
  fun ins (term, s) = (term::s)
    (* if mem(term, s) = False then
      (term::s)
    else
      raise Fail "ins: term already in set" *)

  (* fun rem _ = raise Fail "todo: VarSet.rem" *)
  fun rem (term, s) =
    (case s of
        (t::ts) =>
          if term = t then
            rem(term, ts)
          else 
            (t::(rem(term, ts)))
      | _ => []
    )

  (* fun union _ = raise Fail "todo: VarSet.union" *)
  fun union (s1, s2) =
    (case s1 of
        (s1_t::s1_ts) => 
          if mem(s1_t, s2) = false then
            (* ins(s1_t, (union(s1_ts, s2))) *)
            ins(s1_t, union(s1_ts, s2))
            (* (s1_t::(union(s1_ts, s2))) *)
          else
            ins(s1_t, union(s1_ts, rem(s1_t, s2)))
            (* (s1_t::(union(s1_ts, rem(s1_ts, s2)))) *)
      | _ => s2
    )
				      
end
