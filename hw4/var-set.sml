structure VarSet :> sig

  type set

  val empty  : set
  val mem    : string * set -> bool
  val ins    : string * set -> set
  val rem    : string * set -> set
  val union  : set * set -> set

(* the following operations aren't necessary for substitution *)
(* it just seemed sad not to have them for sets *)
(* they also make testing much easier *)

  (* val sing   : string -> set (* singleton set *)
  val size   : set -> int
  val subset : set * set -> bool
  val equals : set * set -> bool
			      
  val toList : set -> string list *)

end = struct

  type set = string list

  val empty : string list = []

  fun mem (x, []) = false
    | mem (x, y::ys) = x=y orelse mem (x, ys)

  fun ins (x, s) = if mem (x, s) then s else x::s

  fun rem (x, []) = []
    | rem (x, y::ys) = if x=y then ys else y::(rem (x, ys))

  fun union ([], set2) = set2
    | union (x::xs, set2) = union (xs, ins (x, set2))

  (* fun sing _   = raise Fail "todo"
  fun size _   = raise Fail "todo"
  fun subset _ = raise Fail "todo"
  fun equals _ = raise Fail "todo"
  fun toList _ = raise Fail "todo" *)
      				  
end
