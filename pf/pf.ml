(* Code. *)

type sub = 
    Prim  of prim
  | Quote of datum
  | Seq   of sub * sub

and prim = Dup | Drop
and result = 
    Success of stack
  | Error

(* State. *)

and datum =
    Number of int
  | Code   of sub 
  | Stack  of stack

and stack =
    Empty
  | Push of datum * stack

and cont = 
    Done
  | Frame of sub * cont

and state =
    Halt  of result
  | State of stack * cont

;;


(* Code primitives. *)

let apply a  =
  match a with
      (Dup, Push(d, stk)) -> Success(Push(d,Push(d,stk)))
    | (Drop, Push(d, stk)) -> Success(stk)
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
              (match apply(fn, stk) with
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

(* EXAMPLES:

Execute the code "123 dup"

  # run (Seq (Quote (Number 123), Prim Dup)) ;;
  - : result = Success (Push (Number 123, Push (Number 123, Empty)))

Define a word `foo'

  # let foo = (Seq (Quote (Number 123), Prim Dup)) ;;
  val foo : sub = Seq (Quote (Number 123), Prim Dup)

Execute the code "foo 456"

  # run (Seq (foo, Quote (Number 456)));;
  - : result = Success (Push (Number 456, Push (Number 123, Push (Number 123, Empty))))

*)
