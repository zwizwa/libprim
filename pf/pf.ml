(* PF virtual machine.

   The memory model is built as a recursive data type rooted at a
   `state' object.  When reflection is desired, there is also a `dict'
   object mapping symbolic names to `sub' instances.

   In the C implementation, memory is segmented into two regions:
   linear and nonlinear memory, both using different memory management
   facilities.  Note that this Ocaml encoding doesn't reflect this
   fact: it is mainly intended to specify the interpreter.

   Data references are constrained by the following rules:

     L  All data structures in linear memory are flat trees,
        represented as (possibly nested) stacks.  Multiple references
        to the same linear object are not allowed.
   
     NL This excludes references from linear -> nonlinear memory.  As
        a consequence, nonlinear memory can (and needs to) contain
        multiple references and cycles.

   All nonlinear data definitions in this file are annotated as NL.
   The default is linear.  Linearity constraints are not enforced
   statically.
*)


(* REPRESENTATION *)

(* -- Code 

`Run' is not a `prim', because it modifies the continuation directly,
while a `prim' is limited in scope in that it maps a stack -> value.
Note that the `binop' type isn't necessary, but it makes the
interpreter look nicer.  The C version has only one primitive type. *)
type sub =
    Run
  | Nop
  | Abort      
  | Prim    of prim
  | Quote   of datum
  | Seq     of sub * sub (* NL *)
and prim  = Dup | Drop | Choose | Binop of binop
and binop = Multiply | Minus | Equals

(* -- Evaluation result *)
and value = Error of string | Success of stack

(* -- Tagged dynamic data type.  Programmer visible.  In practice this
      contains additional `leaf objects' with an open/close management
      protocol, implemented in C. *)
and datum =
    False 
  | True
  | Number of int
  | Code   of sub 
  | Stack  of stack

(* -- Machine state: contains the two linear stacks necessary to
      complete the current computation, or a halting condition. *)
and state = 
    Halt  of value 
  | State of stack * cont

(* -- Parameter and continuation stacks.  Isomorphic, but not equal. *)
and stack = Empty | Push of datum * stack
and cont  = Done  | Next of sub   * cont
;;



(* VIRTUAL MACHINE *)

(* Simpler constructors for numbers and quoted programs. *)
let lit n    = Quote(Number(n)) ;;
let quot sub = Quote(Code(sub)) ;;

(* Code primitives. *)
let apply a  =
  match a with
      (Dup, Push(d, stk)) -> Success(Push(d,Push(d,stk)))
    | (Drop, Push(d, stk)) -> Success(stk)
    | (Choose, Push(condition, Push(no, Push (yes, stk)))) ->
        (match condition with
             False -> Success(Push(no, stk))
           | _ -> Success(Push(yes, stk)))
    | (Binop (op), Push(Number(r), Push(Number(l), stk))) ->
        Success
          (Push
             ((match op with
                   Multiply -> Number(l * r)
                 | Minus -> Number(l - r)
                 | Equals -> if (l = r) then True else False),
              stk))
    | _ -> Error "invalid argument"

;;
    
(* Composite code interpreter step function. *)
let step s =
  match s with
      Halt(res) -> Halt (res)
    | State(stk, Done) -> Halt (Success(stk))
    | State(stk, Next(sub, k)) ->
        (match sub with
             Nop -> State(stk, k)
           | Abort -> (Halt(Error "abort"))
           | Run -> 
               (match stk with
                    Push(Code(sub), stk) -> State(stk, Next(sub, k))
                  | _ -> Halt(Error "run: stack underflow"))
           | Prim(fn) -> 
               (match apply(fn, stk) with
                    Error(msg) -> Halt (Error(msg))
                  | Success(stack) -> State(stack, k))
           | Quote(dat) -> State(Push(dat,stk), k)
           | Seq(now, later) -> State(stk, Next(now, Next(later, k))))
;;

(* Start execution with an empty parameter stack and a continuation
   frame containing a single subroutine. *)
let run code =
  let rec loop state =
    match state with
        Halt (res) -> res 
      | s -> loop (step s)
  in
    loop (State(Empty, Next(code, Done)))
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

(* Using the analogy of Lisp lists in `dot notation', this converts a
   `proper' source list (a b c d) to an `improper' list (A B C . D) of
   chained Seq pairs.  This ensures that the call to `D' doesn't push
   the contination stack when the Seq instruction is interpreted:
   there is nothing to do after `D'. *)

let rec compile d src =
  match src with
      [] -> Nop
    | [var] -> find d var 
    | var :: s -> Seq(find d var, compile d s)

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




(*
let _if       = Seq(_pick, _run) ;;
let _square   = Seq(_dup, _multiply) ;;
*)

(* Faculty in Factor:
: fac ( n -- n! ) dup 1 = [ 1 ] [ dup 1 - fac ] if * ;
*)

(* 
This doesn't work because it's a recursive _data_ definition: OCaml is
strict, so such things need to be solved using mutation.

let rec _fac =
  Seq(_dup,
  Seq(lit 1,
  Seq(_equals,
  Seq(quot (lit 1),  
  Seq(quot ((Seq(_dup,
             Seq(lit 1,
             Seq(_minus, 
                 _fac))))),
  Seq(_if, 
      _multiply))))));;
*)

(* Test *)


let run_tests d =
  (run (Seq(lit 123, find d "square")),
   run (Seq(lit 10,  Seq(lit 3, find d "-"))),
   run (Seq(lit 1,   Seq(lit 1, find d "="))),
   run (Seq(lit 1,   Seq(lit 2, find d "=")))) ;;

run_tests d2 ;;


