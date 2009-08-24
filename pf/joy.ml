(* Joy virtual machine.

   The point of this code is to illustrate the difference between Joy,
   a highly reflective concatenative language with intensional[1]
   quotations, and PF, an early-bound, linear language with
   extensional quotation.

   In Joy, lists, code, partial applications and continuations are
   represented using the same list data structure (= intensional
   representation).

   In PF, all these constructs are extensional.  It is possible to
   convert lists to code and partial applications, but not the other
   way around.

   The Joy interpreter state machine is represented by a parameter
   stack and a continuation.  Because there is only _one kind_ of
   continuation (code sequencing) it is possible to allow the
   continuation to _be_ a quotation.  (This is in contrast to
   i.e. Scheme, which has different types of ontinuation frames for
   sequencing, parameter evaluation, set!, ...)

   [1] http://www.latrobe.edu.au/philosophy/phimvt/joy/j07rrs.html
*)


(* REPRESENTATION *)


(* Note that the Joy specification doesn't provide an operational
   semantics.  It provides just a (high level) denotational semantics.

   This VM allows the programmer to see a little bit more of the
   operational side, by exposing these high level data primitives that
   are part of the implementation ... *)

type datum = 
    False | True 
  | Number of int 
  | Symbol of string
  | List   of (datum list)

(* ... but not the low level code primitives ... *)

and prim     = Run | Nop | Abort | Stackop of stackop
and stackop  = Dup | Drop | Choose | Binop of binop
and binop    = Multiply | Minus | Equals

(* ... nor the intermediate state of the interpreter.  However, the
   parameter stack and continuation are represented as list
   structures, and can be replaced or modified by the programmer
   through the appropriate impure primitive functions.  *)

and state = 
    Halt  of (datum list)
  | State of (datum list) * (datum list)
and code =
    Prim of prim
  | Quot of datum list
and definition = 
    Def of string * code
      


(* In a purely concatenative language, all code can be fully expanded
   to a possibly infinite sequence of primitive functions.  A
   semantics can then be added by ``doing something'' with this
   sequence.

   - a stack implementation will take each function and apply it to a
     stack data structure (or more generally a state structure if
     continuation modification is allowed).

   - a rewriting engine could transform the stream / list of
     primitives into an irreducible stream / list.
*) 

exception InvalidRunState of state ;;
exception InvalidStackop of stackop * datum list;;

(* In this stack machine VM, evaluation of code boils down to
   recursively expanding the first element in a quotation until it is
   a primitive function and executing it, continuing with the next.
   There are 2 places where this function is called: after resolving a
   symbol to a quotation, and when interpreting the `i' operation. *)

let rec expand quot k =
  match quot with
      [] -> k 
    | dat :: q -> dat :: expand q k
;;
let stackop op stk =
  match (op, stk) with
      (Dup, d :: stk) -> d :: d :: stk
    | (Drop, d :: stk) -> stk
    | (Choose, no :: yes :: condition :: stk) ->
        (match condition with
             False -> no :: stk
           | other -> yes :: stk)
    | (Binop (op), Number(r) :: Number(l) :: stk) ->
        (match op with
              Multiply -> Number(l * r)
            | Minus    -> Number(l - r)
            | Equals   -> if (l = r) then True else False)
        :: stk
    | other -> raise (InvalidStackop (op, stk))
;;  
let op prim state =
  match (prim,state) with
      (Run, State(List(quot) :: stk, k)) -> State(stk, expand quot k)
    | (Stackop(op), State(stk, k)) -> State(stackop op stk, k)
    | _ -> raise (InvalidRunState state)
;;

(* In Joy, code is represented by quotations which can contain symbols
   that refer to quotations stored in a dictionary.  We call such a
   symbol -> quotation reference an `abstraction'. *)

let rec find defs var =
  match defs with
      [] -> raise (Undefined(var))
    | Def(name, code) :: es -> 
        if (name = var) then code else find es var ;;

(* The interpreter step performs expansions (symbol -> quotation list)
   and contractions: application of primitive functions to the machine
   state. *)
let step env state =
  match state with
      Halt(res) -> Halt (res)
    | State(stk, []) -> Halt (stk)
    | State(stk, dat :: k) -> 
        match dat with
            Symbol(name) -> 
              (match (find env name) with
                   Quot quot -> State(stk, expand quot k)
                 | Prim prim -> (op prim (State(stk, k))))
          | other ->
              State(dat :: stk, k)
;;



(* Execution starts with an empty parameter stack and a continuation
   frame containing a single quotation.  The result of the computation
   is the remaining parameter stack. *)
let run env quot =
  let rec loop state =
    match state with
        Halt (res) -> res 
      | s -> loop (step env s)
  in
    loop (State([], quot))
;;



(* Dictionary *)
let dict =
  [Def ("dup",    Prim (Stackop Dup));
   Def ("drop",   Prim (Stackop Drop));
   Def ("choose", Prim (Stackop Choose));
   Def ("*",      Prim (Stackop (Binop Multiply)));
   Def ("-",      Prim (Stackop (Binop Minus)));
   Def ("=",      Prim (Stackop (Binop Equals)));
   Def ("i",      Prim Run);
   Def ("square", Quot ([Symbol("dup"); Symbol("*")]));
   Def ("if",     Quot ([Symbol("choose"); Symbol("i")]));
   Def ("fac",    Quot ([Symbol("dup");
                         Number(1);
                         Symbol("=");
                         List([Number(1)]);
                         List([Symbol("dup");
                               Number(1);
                               Symbol("-");
                               Symbol("fac")]);
                         Symbol("if");
                         Symbol("*")]));
                                
  ];;




let test dict =
  (run dict [Number(1); Symbol("dup")],
   run dict [Number(12);  Symbol("square")],
   run dict [List([Number(123)]); Symbol("i")],
   run dict [Number(6); Symbol("fac")]
  );;

test dict ;;
