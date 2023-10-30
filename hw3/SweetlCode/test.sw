:plus = [m [n [s [z ((m s) ((n s) z))]]]];
:mult = [m [n ((m (:plus n)) 0)]];
:pair = [f [s [b ((b f) s)]]];
:fst = [p (p @t)];
:snd = [p (p @f)];
:zz = ((:pair 0) 0);
:ss = [p ((:pair (:snd p)) ((:plus 1) (:snd p)))];
:prd = [m (:fst ((m :ss) :zz))];
:iszero = [m ((m [x @f]) @t)];
:cond = [b [x [y ((b x) y)]]];
:and = [x [y (((:cond x) y) @f)]];
:minus = [m [n ((n :prd) m)]];
:equals = [m [n (:iszero ((:minus m) n))]];
(:iszero ((:minus 20) 20))