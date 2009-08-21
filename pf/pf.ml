(* Code is nonlinear. *)
type sub = 
    Prim  of prim
  | Quote of datum
  | Seq   of sub * sub

and prim =
    Address of int

(* State is linear. *)
and k = 
    Done
  | Frame of sub * k

and datum =
    Number of int
  | Code   of sub 
  | Stack  of stack

and stack =
    Empty
  | Push of datum * stack

and state =
    Halt  of stack
  | State of stack * k

;;




let run s =
  match s with
      Halt (p) -> Halt (p)
    | State (p, Done) -> Halt (p)
    | State (p, Frame (sub, knext)) ->
        match sub with
            Prim (p) -> s
          | Seq (now, next) -> s
          | Quote (d) -> State(Push(d,p), knext)
;;
          
    
