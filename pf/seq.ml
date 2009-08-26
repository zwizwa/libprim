(* Nonlinear variant of the PF virtual machine.

   This is something inbetween joy.ml and pf.ml

   Essentially, Joy with extensional quotations, or PF without linear
   run-time memory.
*)


(* REPRESENTATION *)

(* -- Code 

Code is either a primitive node or a binary tree of code.  The fringe
of this binary tree is the sequence of primitives that implement its
behaviour.

`Run' is not a `prim', because it modifies the continuation directly,
while a `prim' is limited in scope in that it maps a stack -> value.
Note that the `binop' type isn't necessary, but it makes the
interpreter look nicer. *)
type sub =
    Run | Nop | Abort | Done
  | Prim    of prim
  | Quote   of datum
  | Seq     of sub * sub (* NL *)
and prim  = Dup | Drop | Choose | Binop of binop
and binop = Multiply | Minus | Equals

(* -- Evaluation result *)
and value = Error of string | OK of stack

(* -- Programmer visible data is represented as a universal (dynamic) type. *)
and datum = 
    False | True 
  | Number of int 
  | Code of sub 
  | Stack of stack

(* -- Machine state: contains the parameter stack and the continuation
      in the form of a `sub'. *)
and state = 
    Halt  of value 
  | State of stack * sub

(* -- Parameter stack. *)
and stack = 
    Empty 
  | Push of datum * stack



(* VIRTUAL MACHINE *)

(* Simpler constructors for numbers and quoted programs. *)
let lit n    = Quote(Number(n)) ;;
let quot sub = Quote(Code(sub)) ;;

(* Code primitives. *)
let apply a  =
  match a with
      (Dup, Push(d, stk)) -> OK(Push(d,Push(d,stk)))
    | (Drop, Push(d, stk)) -> OK(stk)
    | (Choose, Push(no, Push(yes, Push (condition, stk)))) ->
        (match condition with
             False -> OK(Push(no, stk))
           | other -> OK(Push(yes, stk)))
    | (Binop (op), Push(Number(r), Push(Number(l), stk))) ->
        OK(Push
             ((match op with
                   Multiply -> Number(l * r)
                 | Minus    -> Number(l - r)
                 | Equals   -> if (l = r) then True else False),
              stk))
    | other -> Error "invalid argument"
;;
    
(* Composite code interpreter step function.  Code sequences are
   interpreted by incremental flattening through binary tree
   rotation. *)

let step s =
  match s with
      Halt(res) -> Halt (res)
    | State(stk, Done) -> Halt (OK(stk))
    | State(stk, Seq(sub, k))  ->
        (match sub with
             Nop -> State(stk, k)
           | Abort -> (Halt(Error "abort"))
           | Run -> 
               (match stk with
                    Push(Code(sub), stk) -> State(stk, Seq(sub, k))
                  | other -> Halt(Error "run: invalid argument"))
           | Prim(fn) -> 
               (match apply(fn, stk) with
                    Error(msg) -> Halt (Error(msg))
                  | OK(stack) -> State(stack, k))
           | Quote(dat) -> State(Push(dat,stk), k)
           | Seq(now, later) -> State(stk, Seq(now, Seq(later, k))))
;;

(* Start execution with an empty parameter stack and a continuation
   frame containing a single subroutine. *)
let run code =
  let rec loop state =
    match state with
        Halt (res) -> res 
      | s -> loop (step s)
  in
    loop (State(Empty, Seq(code, Done)))
;;


(* COMPILATION: src -> sub *)

(* Dictionary *)
type entry = Entry of string * sub ;;
exception Undefined of string ;;

let rec find entries var =
  match entries with
      [] -> raise (Undefined(var))
    | Entry(name, sub) :: es -> 
        if (name = var) then sub else find es var ;;

let interpret entries str =
  try find entries str
  with Undefined(var) -> Quote(Number(int_of_string var)) ;;

(* Using the analogy of Lisp lists in `dot notation', this converts a
   `proper' source list (a b c d) to an `improper' list (A B C . D) of
   chained Seq pairs.  This ensures that the call to `D' doesn't push
   the contination stack when the Seq instruction is interpreted:
   there is nothing to do after `D'. *)

let rec compile d src =
  match src with
      [] -> Nop
    | [var] -> interpret d var 
    | var :: s -> Seq(interpret d var, compile d s)

(* Bootstrap dictionary with primitive stack and machine transformers. *)
let d1 =
  [Entry ("dup",    Prim Dup);
   Entry ("drop",   Prim Drop);
   Entry ("choose", Prim Choose);
   Entry ("*",      Prim (Binop Multiply));
   Entry ("-",      Prim (Binop Minus));
   Entry ("=",      Prim (Binop Equals));
   Entry ("run",    Run)];;

(* Add highlevel library code *)
let d2 =
  Entry ("if",     compile d1 ["choose"; "run"]) ::
  Entry ("square", compile d1 ["dup"; "*"]) ::
  d1 ;;


(* Test *)
let run_tests d =
  (run (compile d ["12"; "square"]),
   run (compile d ["10"; "3"; "-"]),
   run (compile d ["1"; "1"; "="]),
   run (compile d ["1"; "2"; "="])) 
;;
run_tests d2 ;;

