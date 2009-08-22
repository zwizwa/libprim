(* PF virtual machine.

   The memory model is built as a recursive data type rooted at a
   `state' object.  When reflection is desired, there is also a `dict'
   object mapping symbolic names to `sub' instances.

   In the C implementation, memory is segmented into two regions:
   linear and nonlinear memory, both using different memory management
   facilities.  Note that this Ocaml encoding doesn't reflect this
   fact: it is mainly intended to specify the interpreter.

   Data references are constrained by the following rules:

     L  Multiple references _to_ the same linear object are not
        allowed.  All data structures in linear memory are flat trees,
        represented as (possibly nested) stacks.
   
     NL There are no restrictions on references _to_ nonlinear
        objects.  Nonlinear memory can (and needs to) contain multiple
        references and cycles.

   Nonlinear objects cannot refer to linear ones, but the other way is
   not a problem.  To linear memory, nonlinear data structures behave
   as constants.

   All nonlinear data definitions in this file are annotated as NL.
   Currently there is only one: `sub', representing a compiled
   subroutine.  The default is linear.

   Linearity constraints are not enforced statically.

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
and value = Error of string | OK of stack

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
    
(* Composite code interpreter step function. *)
let step s =
  match s with
      Halt(res) -> Halt (res)
    | State(stk, Done) -> Halt (OK(stk))
    | State(stk, Next(sub, k)) ->
        (match sub with
             Nop -> State(stk, k)
           | Abort -> (Halt(Error "abort"))
           | Run -> 
               (match stk with
                    Push(Code(sub), stk) -> State(stk, Next(sub, k))
                  | other -> Halt(Error "run: invalid argument"))
           | Prim(fn) -> 
               (match apply(fn, stk) with
                    Error(msg) -> Halt (Error(msg))
                  | OK(stack) -> State(stack, k))
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


(* Recursion. 

   This requires a recursive data structure.

   Until I figure out how to do this in the `compile' function (would
   require two passes and data structure mutation) the recursive code
   structure is created here using `let rec'.

   Because this form can only pierce through 1 layer of constructors
   to patch up the graph structure, a number of intermediate nodes are
   defined.  See here for more info:

   http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#s:letrecvalues

   The definition of the `fac' word in Factor:

      : fac ( n -- n! ) dup 1 = [ 1 ] [ dup 1 - fac ] if * ;

   Note also that below is more like an encoding of:

      : dup1- dup 1 - ;
      : fac   dup 1 = [ 1 ] [ dup1- fac ] if * ;

   It's of course equivalent: the corresponding Seq trees have the
   same fringe, but their structure is different.


*)

let rec _fac  = Seq(compile d2 ["dup"; "1"; "="], n1)
and      n1   = Seq(Quote(Code(compile d2 ["1"])), n5)
and      n2   = Seq(compile d2 ["dup"; "1"; "-"], _fac)
and      n3   = Code(n2)
and      n4   = Quote(n3)
and      n5   = Seq(n4, n6)
and      n6   = compile d2 ["if"; "*"]
;;

let d3 = Entry ("fac", _fac) :: d2 ;;

(*
# run (compile d3 ["6"; "fac"]) ;;
- : value = OK (Push (Number 720, Empty))
*)

