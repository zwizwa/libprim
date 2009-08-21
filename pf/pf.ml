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


(* Primitive code interpreter.  Note: Prim can't have a type: state ->
   state (no GADTs in OCaml), so Prim consists of tags which are
   interpreted explicitly. *)

let call a  =
  match a with
      App (Dup, Push(d, stk)) -> Success(Push(d,Push(d,stk)))
    | App (Drop, Push(d, stk)) -> Success(stk)
    | _ -> Error
;;
    
(* Composite code interpreter step function. *)

let step s =
  match s with
      Halt (res) -> Halt (res)
    | State (stk, Done) -> Halt (Success (stk))
    | State (stk, Frame (sub, k)) ->
        match sub with
            Prim (fn) -> 
              (match call(App(fn, stk)) with
                   Error -> Halt (Error)
                 | Success(stack) -> State(stack, k))
          | Quote (dat) -> State(Push(dat,stk), k)
          | Seq (now, next) -> State(stk, Frame(now, Frame(next, k)))
;;

(* Start execution with an empty parameter stack and a continuation
   frame containing a single subroutine. *)

let run code =
  let rec loop state =
    match state with
        Halt (res) -> res 
      | s -> loop (step s)
  in
    loop (State(Empty, Frame(code, Done)))
;;

(* EXAMPLE:

# run (Seq (Quote (Number 123), Prim Dup)) ;;
- : result = Success (Push (Number 123, Push (Number 123, Empty)))

*)
