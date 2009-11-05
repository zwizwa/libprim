/* s-expression parser adapted from tinyscheme (LGPL) */

/* T I N Y S C H E M E    1 . 3 9
 *   Dimitrios Souflis (dsouflis@acm.org)
 *   Based on MiniScheme (original credits follow)
 * (MINISCM)               coded by Atsushi Moriwaki (11/5/1989)
 * (MINISCM)           E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 * (MINISCM) This version has been modified by R.C. Secrist.
 * (MINISCM)
 * (MINISCM) Mini-Scheme is now maintained by Akira KIDA.
 * (MINISCM)
 * (MINISCM) This is a revised and modified version by Akira KIDA.
 * (MINISCM)	current version is 0.85k4 (15 May 1994)
 *
 */

#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_SHARP_CONST 11
#define TOK_VEC     12


/* read string expression "xxx...xxx" */
static pointer readstrexp(scheme *sc) {
  char *p = sc->strbuff;
  int c;
  int c1=0;
  enum { st_ok, st_bsl, st_x1, st_x2, st_oct1, st_oct2, st_oct3 } state=st_ok;
  
  for (;;) {
    c=inchar(sc);
    if(c==EOF || p-sc->strbuff>sizeof(sc->strbuff)-1) {
      return sc->F;
    }
    switch(state) {
        case st_ok:
            switch(c) {
                case '\\':
	                state=st_bsl;
	                break;
                case '"':
	                *p=0;
	                return mk_counted_string(sc,sc->strbuff,p-sc->strbuff);
                default:
	                *p++=c;
	                break;
            }
            break;
        case st_bsl:
            switch(c) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                        state=st_oct1;
                        c1=c-'0';
                        break;
                case 'x':
                case 'X':
	                state=st_x1;
	                c1=0;
	                break;
                case 'n':
	                *p++='\n';
	                state=st_ok;
	                break;
                case 't':
	                *p++='\t';
	                state=st_ok;
	                break;
                case 'r':
	                *p++='\r';
	                state=st_ok;
	                break;
                case '"':
	                *p++='"';
	                state=st_ok;
	                break;
                default:
	                *p++=c;
	                state=st_ok;
	                break;
            }
            break;
        case st_x1:
        case st_x2:
            c=toupper(c);
            if(c>='0' && c<='F') {
	            if(c<='9') {
	                c1=(c1<<4)+c-'0';
	            } else {
	                c1=(c1<<4)+c-'A'+10;
	            }
	            if(state==st_x1) {
	                state=st_x2;
	            } else {
	                *p++=c1;
	                state=st_ok;
	            }
            } else {
	            return sc->F;
            }
            break;
        case st_oct1:
        case st_oct2:
        case st_oct3:
            if (c < '0' || c > '7')
            {
                   if (state==st_oct1)
                   return sc->F;

                   *p++=c1;
                   backchar(sc, c);
                   state=st_ok;
            }
            else
            {
                   c1=(c1<<3)+(c-'0');
                   switch (state)
                   {
                   case st_oct1:
                        state=st_oct2;
                        break;
                   case st_oct2:
                        state=st_oct3;
                        break;
                   default:
                        *p++=c1;
                        state=st_ok;
                        break;
                   }
            }
            break;

    }
  }
}

/* check c is in chars */
static INLINE int is_one_of(char *s, int c) {
     if(c==EOF) return 1;
     while (*s)
          if (*s++ == c)
               return (1);
     return (0);
}

/* skip white characters */
static INLINE void skipspace(scheme *sc) {
     int c;
     while (isspace(c=inchar(sc)))
          ;
     if(c!=EOF) {
          backchar(sc,c);
     }
}

/* get token */
static int token(scheme *sc) {
     int c;
     skipspace(sc);
     switch (c=inchar(sc)) {
     case EOF:
          return (TOK_EOF);
     case '(':
          return (TOK_LPAREN);
     case ')':
          return (TOK_RPAREN);
     case '.':
          c=inchar(sc);
          if(is_one_of(" \n\t",c)) {
               return (TOK_DOT);
          } else {
               backchar(sc,c);
	       backchar(sc,'.');
               return TOK_ATOM;
          }
     case '\'':
          return (TOK_QUOTE);
     case ';':
           while ((c=inchar(sc)) != '\n' && c!=EOF)
             ;
           return (token(sc));
     case '"':
          return (TOK_DQUOTE);
     case BACKQUOTE:
          return (TOK_BQUOTE);
     case ',':
         if ((c=inchar(sc)) == '@') {
               return (TOK_ATMARK);
         } else {
               backchar(sc,c);
               return (TOK_COMMA);
         }
     case '#':
          c=inchar(sc);
          if (c == '(') {
               return (TOK_VEC);
          } else if(c == '!') {
               while ((c=inchar(sc)) != '\n' && c!=EOF)
                   ;
               return (token(sc));
          } else {
               backchar(sc,c);
               if(is_one_of(" tfodxb\\",c)) {
                    return TOK_SHARP_CONST;
               } else {
                    return (TOK_SHARP);
               }
          }
     default:
          backchar(sc,c);
          return (TOK_ATOM);
     }
}
     case OP_RDSEXPR:
          switch (sc->tok) {
          case TOK_EOF:
               if(sc->inport==sc->loadport) {
                    sc->args=sc->NIL;
                    s_goto(sc,OP_QUIT);
               } else {
                    s_return(sc,sc->EOF_OBJ);
               }
/*
 * Commented out because we now skip comments in the scanner
 * 
          case TOK_COMMENT: {
               int c;
               while ((c=inchar(sc)) != '\n' && c!=EOF)
                    ;
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          } 
*/
          case TOK_VEC:
               s_save(sc,OP_RDVEC,sc->NIL,sc->NIL);
               /* fall through */
          case TOK_LPAREN:
               sc->tok = token(sc);
               if (sc->tok == TOK_RPAREN) {
                    s_return(sc,sc->NIL);
               } else if (sc->tok == TOK_DOT) {
                    Error_0(sc,"syntax error: illegal dot expression");
               } else {
                    sc->nesting_stack[sc->file_i]++;
                    s_save(sc,OP_RDLIST, sc->NIL, sc->NIL);
                    s_goto(sc,OP_RDSEXPR);
               }
          case TOK_QUOTE:
               s_save(sc,OP_RDQUOTE, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_BQUOTE:
               sc->tok = token(sc);
	       if(sc->tok==TOK_VEC) {
		 s_save(sc,OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
		 sc->tok=TOK_LPAREN;
		 s_goto(sc,OP_RDSEXPR);
	       } else {
		 s_save(sc,OP_RDQQUOTE, sc->NIL, sc->NIL);
	       }
               s_goto(sc,OP_RDSEXPR);
          case TOK_COMMA:
               s_save(sc,OP_RDUNQUOTE, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_ATMARK:
               s_save(sc,OP_RDUQTSP, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_ATOM:
               s_return(sc,mk_atom(sc, readstr_upto(sc, "();\t\n\r ")));
          case TOK_DQUOTE:
               x=readstrexp(sc);
	       if(x==sc->F) {
		 Error_0(sc,"Error reading string");
	       }
               setimmutable(x);
               s_return(sc,x);
          case TOK_SHARP: {
               pointer f=find_slot_in_env(sc,sc->envir,sc->SHARP_HOOK,1);
               if(f==sc->NIL) {
                    Error_0(sc,"undefined sharp expression");
               } else {
                    sc->code=cons(sc,slot_value_in_env(f),sc->NIL); 
                    s_goto(sc,OP_EVAL);
               }
          }
          case TOK_SHARP_CONST:
               if ((x = mk_sharp_const(sc, readstr_upto(sc, "();\t\n\r "))) == sc->NIL) {
                    Error_0(sc,"undefined sharp expression");
               } else {
                    s_return(sc,x);
               }
          default:
               Error_0(sc,"syntax error: illegal token");
          }
          break;

     case OP_RDLIST: {
          sc->args = cons(sc, sc->value, sc->args);
          sc->tok = token(sc);
/* We now skip comments in the scanner

          while (sc->tok == TOK_COMMENT) {
               int c;
               while ((c=inchar(sc)) != '\n' && c!=EOF)
                    ;
               sc->tok = token(sc);
          }
*/
          if (sc->tok == TOK_RPAREN) {
               int c = inchar(sc);
               if (c != '\n') backchar(sc,c);
               sc->nesting_stack[sc->file_i]--;
               s_return(sc,reverse_in_place(sc, sc->NIL, sc->args));
          } else if (sc->tok == TOK_DOT) {
               s_save(sc,OP_RDDOT, sc->args, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          } else {
               s_save(sc,OP_RDLIST, sc->args, sc->NIL);;
               s_goto(sc,OP_RDSEXPR);
          }
     }

     case OP_RDDOT:
          if (token(sc) != TOK_RPAREN) {
               Error_0(sc,"syntax error: illegal dot expression");
          } else {
               sc->nesting_stack[sc->file_i]--;
               s_return(sc,reverse_in_place(sc, sc->value, sc->args));
          }

     case OP_RDQUOTE:
          s_return(sc,cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDQQUOTE:
          s_return(sc,cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDQQUOTEVEC:
       s_return(sc,cons(sc, mk_symbol(sc,"apply"),
			cons(sc, mk_symbol(sc,"vector"), 
			     cons(sc,cons(sc, sc->QQUOTE, 
				  cons(sc,sc->value,sc->NIL)),
				  sc->NIL))));

     case OP_RDUNQUOTE:
          s_return(sc,cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDUQTSP:
          s_return(sc,cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));

     case OP_RDVEC:
          /*sc->code=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
          s_goto(sc,OP_EVAL); Cannot be quoted*/
       /*x=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
	 s_return(sc,x); Cannot be part of pairs*/
       /*sc->code=mk_proc(sc,OP_VECTOR);
       sc->args=sc->value;
       s_goto(sc,OP_APPLY);*/
       sc->args=sc->value;
       s_goto(sc,OP_VECTOR);
