structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next [] = NONE
    | next (#"Z" :: cs) = SOME(T.Z, cs)
    | next (#"T" :: cs) = SOME(T.T, cs)
    | next (#"F" :: cs) = SOME(T.F, cs)
    | next (#"S" :: cs) = SOME(T.S, cs)
    | next (#"P" :: cs) = SOME(T.P, cs)
    | next (#"[" :: cs) = SOME(T.LBrack, cs)
    | next (#"]" :: cs) = SOME(T.RBrack, cs)
    | next (#"+" :: cs) = SOME(T.Plus, cs)
    | next (#"-" :: cs) = SOME(T.Minus, cs)
    | next (#"<" :: cs) = SOME(T.LessThan, cs)
    | next (#">" :: cs) = SOME(T.GreaterThan, cs)
    | next (#"&" :: #"&" :: cs) = SOME(T.DoubleAmpersand, cs)
    | next (#"|" :: #"|" :: cs) = SOME(T.DoublePipe, cs)
    | next (#"?" :: cs) = SOME(T.QuestionMark, cs)
    | next (#":" :: cs) = SOME(T.Colon, cs)
    | next (c :: cs) = if Char.isSpace c
                        then next cs
                        else raise Fail "invalid token"

  fun scan code =
    let
      fun lp [] = []
        | lp chars = (case next chars of 
                          SOME(tok, chars') => (tok :: lp chars')
                        | NONE => []
                      )
    in
      lp(explode code)
    end

end
