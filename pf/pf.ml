(* Code. *)

type sub = 
    Run
  | Prim    of prim
  | Quote   of datum
  | Seq     of sub * sub

and binop = Multiply | Minus | Equals
and prim  = Dup | Drop | Pick | Binop of binop
and value = 
    Error of string
  | Success of stack


(* Notes:
   
   - `Run' is not a `prim', because it modifies the continuation
     directly, while a `prim' maps a stack -> value. 

   - The binop type isn't necessary, but it makes the interpreter look
     nicer.  The C version has only one primitive type.
*)


(* State. *)

and datum =
    False
  | True
  | Number of int
  | Code   of sub 
  | Stack  of stack

and stack =
    Empty
  | Push of datum * stack

and cont = 
    Done
  | Frame of sub * cont

and state =
    Halt  of value
  | State of stack * cont

;;

(* Simpler constructor for numbers and quoted programs. *)
let lit n   = Quote(Number(n)) ;;
let quot sub  = Quote(Code(sub)) ;;

(* Code primitives. *)
let apply a  =
  match a with
      (Dup, Push(d, stk)) -> Success(Push(d,Push(d,stk)))
    | (Drop, Push(d, stk)) -> Success(stk)
    | (Pick, Push(condition, Push(no, Push (yes, stk)))) ->
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
    | State(stk, Frame(sub, k)) ->
        (match sub with
             Run -> 
               (match stk with
                    Push(Code(sub), stk) -> State(stk, Frame(sub, k))
                  | _ -> Halt(Error "run: stack underflow"))
           | Prim(fn) -> 
               (match apply(fn, stk) with
                    Error(msg) -> Halt (Error(msg))
                  | Success(stack) -> State(stack, k))
           | Quote(dat) -> State(Push(dat,stk), k)
           | Seq(now, next) -> State(stk, Frame(now, Frame(next, k))))
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


(* Dictionary *)

(* Bootstrap dictionary with primitive stack and machine transformers. *)
let _dup      = Prim Dup ;;
let _drop     = Prim Drop ;;
let _pick     = Prim Pick ;;
let _multiply = Prim (Binop Multiply) ;;
let _minus    = Prim (Binop Minus) ;;
let _equals   = Prim (Binop Equals) ;;
let _run      = Run ;;

(* Highlevel library code *)
let _if       = Seq(_pick, _run) ;;
let _square   = Seq(_dup, _multiply) ;;

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
let run_tests =
  (run (Seq(lit 123, _square)),
   run (Seq(lit 10, Seq(lit 3, _minus))),
   run (Seq(lit 1, Seq(lit 1, _equals))),
   run (Seq(lit 1, Seq(lit 2, _equals))))
;;


(* EXAMPLES:

Execute the code "123 dup"

  # run (Seq (Quote (Number 123), Prim Dup)) ;;
  - : value = Success (Push (Number 123, Push (Number 123, Empty)))

Define a word `foo'

  # let foo = (Seq (Quote (Number 123), Prim Dup)) ;;
  val foo : sub = Seq (Quote (Number 123), Prim Dup)

Execute the code "foo 456"

  # run (Seq (foo, Quote (Number 456)));;
  - : value = Success (Push (Number 456, Push (Number 123, Push (Number 123, Empty))))

*)
