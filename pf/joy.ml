(* Joy virtual machine.

   The point of this code is to illustrate the difference between Joy
   a highly reflective concatenative language, and PF, an early-bound,
   linear language with limited reflection.

   In Joy, lists, code, partial applications and continuations are
   represented using the same list data structure (= Intensional
   representation).  In PF, all these are extensional.  It is possible
   to convert lists to code and partial applications, but not the
   other way around.  I.e. these are all abstract objects, which
   allows them to be compiled (reduced before the program runs) and
   provide some guarantees about the behaviour of a program
   (i.e. linear memory usage).

   The Joy interpreter state machine is represented by a parameter
   stack and a continuation.  Note that because there is only one kind
   of continuation(as opposed to Scheme i.e.) it is possible to allow
   the continuation to _be_ a quotation.  In CEK machine terminology:
   the current reducable expression (C) and the continuation (K) are
   the same thing.  Also, there is only a global environment (E).

*)


(* REPRESENTATION *)


(* Note that the Joy specification doesn't provide an operational
   semantics.  It provides just a (high level) denotational semantics.

   This VM allows the programmer to see at least some part of the
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
   parameter stack and continuation are represented as `list'
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
      


(* In a concatenative language, all code can be `fully expanded' to a
   possibly infinite sequence of primitive functions.

   An interpreter for a concatenative language takes a particular data
   structure and turns it into an (infinite) sequence of primitive
   functions, which can then be applied to a stack (stack machine
   implementation), or reduced according to a set of primitive
   reduction rules (a rewriting machine implementation).

   In Joy, code is represented by quotations which can contain symbols
   that refer to quotations stored in a dictionary.  We call such a
   symbol -> quotation reference an `abstraction'.

   In this stack machine VM, evaluation of code boils down to
   recursively expanding the first element in a quotation until it is
   a primitive function and executing it, continuing with the next.

   In PF, the expansion happens in two steps.  Compilation converts an
   environment and a symbolic code tree into a binary graph which is
   then flattened into a sequence of primitives at run time.

*) 

exception Error of string ;;
exception InvalidStackop of stackop * datum list;;

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
    | _ -> raise (Error "invalid argument (run)")
;;

let rec find defs var =
  match defs with
      [] -> raise (Undefined(var))
    | Def(name, code) :: es -> 
        if (name = var) then code else find es var ;;

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



(* Start execution with an empty parameter stack and a continuation
   frame containing a single subroutine. *)
let run env code =
  let rec loop state =
    match state with
        Halt (res) -> res 
      | s -> loop (step env s)
  in
    loop (State([], code))
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
