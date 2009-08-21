
type sub  = 
    Prim  of prim
  | Quote of datum
  | Seq   of sub * sub

and k = 
    MT 
  | Frame of sub * k

and datum =
    Number of int
  | Code of sub 
  | Pair of pair

and prim =
    Address of int

and pair =
    Nil
  | Cons of datum * datum

and state =
    State of pair * k

;;


(* let run state =
  match state *)
