(* Code is nonlinear. *)

type sub = 
    Prim  of prim
  | Quote of datum
  | Seq   of sub * sub

and prim = Dup | Drop
and app = App of prim * stack
and result = 
    Success of stack
  | Error

(* State is linear. *)
and datum =
    Number of int
  | Code   of sub 
  | Stack  of stack

and stack =
    Empty
  | Push of datum * stack

and k = 
    Done
  | Frame of sub * k

and state =
    Halt  of result
  | State of stack * k

;;


let doprim a  =
  match a with
      App (Dup, Push(d, p)) -> Success(Push(d,Push(d,p)))
    | App (Dup, p) -> Error
    | _ -> Error
;;
    

let run s =
  match s with
      Halt (res) -> Halt (res)
    | State (stk, Done) -> Halt (Success (stk))
    | State (stk, Frame (sub, knext)) ->
        match sub with
            Prim (fn) -> 
              (match doprim(App(fn, stk)) with
                   Error -> Halt (Error)
                 | Success(stack) -> State(stack, knext))
          | Seq (now, next) -> s
          | Quote (dat) -> State(Push(dat,stk), knext)
;;

