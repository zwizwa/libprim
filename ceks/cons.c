#include <stdlib.h>

typedef union {
    struct _cons *cons;
    void *data;
} obj;

struct _cons {
    obj car;
    obj cdr;
};

typedef struct {
    obj C;  // code
    obj E;  // environment
    obj K;  // continuation
//    obj S;  // store
} machine;

typedef struct {
    machine m;
} sc;

obj obj_alloc(sc *sc, int bytes) {
    return (obj)malloc(bytes);
}

obj cons(sc *sc, obj car, obj cdr) {
    obj x = obj_alloc(sc, sizeof(struct _cons));
    x.cons->car = car;
    x.cons->cdr = cdr;
    return x;
}


// ------------


/* A closure is a pair of ( open-term . environment ) 
   An environment is a function mapping variables to closures.

   Chapter 6 Machines, p 74 in PLLC.
*/

#define CONT_FUN 1
#define CONT_ARG 2
#define CONT_OPD 3
typedef struct {
    int tag;
    
} cont;

void apply(sc *sc, obj M, obj N) {
    
    
}
