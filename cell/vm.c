/* VM on top of CELL.  

   The main idea behind the CELL graph memory is to run on ARM cores
   with little memory (around 32-128 kB).

   For the interpreter it would be convenient to:
     - have compact byte code
     - run code in graph memory
     - run code in flat (constant) Flash memory

   The core language is ANF[1], possibly written in `let*' form which
   collapses nested `let' in one form.


   EXP ::= (app VAL VAL ...)
        |  (let (VAR EXP) EXP)
        |  (begin EXP EXP)
        |  (set! VAR VAL)
        |  (if VAL EXP EXP)

   VAL ::= (lambda (VAR ...) EXP)
        |  VAR



   Then the core language.  Is ANF[1] good enough?

   What about a modification of ANF, the let* form?

   [1] http://en.wikipedia.org/wiki/Administrative_normal_form

*/
