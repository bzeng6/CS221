structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST

  fun next [] = NONE
    | next (T.T :: toks) = SOME(A.True, toks)
    | next (T.F :: toks) = SOME(A.False, toks)
    | next (T.Z :: toks) = SOME(A.Zero, toks)
    | next (T.S :: toks) = 
      (case next toks of
          SOME(t1, toks') => SOME(A.Succ(t1), toks')
        | _ => raise Fail "could not parse succ"
      )
    | next (T.P :: toks) = 
      (case next toks of
          SOME(t1, toks') => SOME(A.Pred(t1), toks')
        | _ => raise Fail "could not parse pred"
      )
    | next (T.LBrack :: toks) = 
      (case next toks of 
          SOME(t1, T.Plus :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.Add(t1, t2), toks'')
                | _ => raise Fail "could not parse plus"
            )
        | SOME(t1, T.Minus :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.Subtract(t1, t2), toks'')
                | _ => raise Fail "could not parse minus"
            )
        | SOME(t1, T.LessThan :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.Less(t1, t2), toks'')
                | _ => raise Fail "could not parse less than"
            )
        | SOME(t1, T.GreaterThan :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.Greater(t1, t2), toks'')
                | _ => raise Fail "could not parse greater than"
            )
        | SOME(t1, T.DoubleAmpersand :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.And(t1, t2), toks'')
                | _ => raise Fail "could not parse double ampersand"
            )
        | SOME(t1, T.DoublePipe :: toks') =>
            (case next toks' of
                SOME(t2, T.RBrack :: toks'') => SOME(A.Or(t1, t2), toks'')
                | _ => raise Fail "could not parse double pipe"
            )
        | SOME(t1, T.QuestionMark :: toks') =>
            (case next toks' of
                SOME(t2, T.Colon :: toks'') => 
                  (case next toks'' of
                      SOME(t3, T.RBrack :: toks''') => SOME(A.Cond(t1, t2, t3), toks''')
                    | _ => raise Fail "could not parse colon"
                  )
                | _ => raise Fail "could not parse question mark"
            )
        | NONE => raise Fail "missing right bracket"
        | _ => raise Fail "not a parseable token"
      )
    | next _ = raise Fail "not a parseable token"


  fun parse toks =
    let 
      fun ps toks = 
        (case next toks of
            SOME(t, ts) => 
              if ts = [] orelse ts = [T.RBrack]
                then t 
                else raise Fail "parse syntax error"
            | _ => raise Fail "parse error"
        )
    in
      ps(toks)
    end
		     
end
