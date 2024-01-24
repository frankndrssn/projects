%token <int> INTLIT
%token <string> VAR
%token TIC
%token OPAREN
%token CPAREN
%token OSQ
%token CSQ
%token PLUS
%token MIN
%token TIMES
%token LT
%token EQL
%token EQ
%token TRUE
%token FALSE
%token IF
%token FUN
%token COLON
%token CONS
%token MATCH
%token DOT
%token FST
%token SND
%token LEFT
%token RIGHT
%token CASE
%token LAMBDA
%token INTTY
%token BOOL
%token ATOM
%token EITHER
%token ARROW
%token LIST
%token TYPEOF
%token DEF
%token EVAL
%token QUIT
%start <Ast.expr> expr
%type <Ast.bop> bop
%start <Ast.typ> typ
%start <Ast.command> com
%type <Ast.expr list> expr_list
%%

expr:
  | OPAREN; b = bop; e1 = expr; e2 = expr; CPAREN                                                                                            { Binop (b, e1, e2)              }
  | TRUE                                                                                                                                     { True                           }
  | FALSE                                                                                                                                    { False                          }
  | n = INTLIT                                                                                                                               { Lit n                          }
  | OPAREN; IF; e1 = expr; e2 = expr; e3 = expr; CPAREN                                                                                      { If (e1, e2, e3)                }
  | OPAREN; e1 = expr; e2 = expr; CPAREN                                                                                                     { App (e1, e2)                   }
  | OPAREN; FUN; OPAREN; f = VAR; OPAREN; x = VAR; COLON; t = typ; CPAREN; COLON; s = typ; CPAREN; e = expr; CPAREN                          { Fun (f, x, t, s, e)            }
  | TIC; OSQ; t = typ; CSQ; OPAREN; es = expr_list; CPAREN                                                                                   { List (t, es)                   }
  | OPAREN; CONS; e1 = expr; e2 = expr; CPAREN                                                                                               { Cons (e1, e2)                  }
  | OPAREN; MATCH; e1 = expr; OPAREN; e2 = expr; CPAREN; OPAREN; OPAREN; x = VAR; y = VAR; CPAREN; e3 = expr; CPAREN; CPAREN                 { MatchList (e1, e2, x, y, e3)   }
  | TIC; OPAREN; e1 = expr; DOT; e2 = expr; CPAREN                                                                                           { Pair(e1, e2)                   }
  | OPAREN; FST; e = expr; CPAREN                                                                                                            { Fst e                          }
  | OPAREN; SND; e = expr; CPAREN                                                                                                            { Snd e                          }
  | OPAREN; LEFT; e = expr; t = typ; CPAREN                                                                                                  { Left (e, t)                    }
  | OPAREN; RIGHT; t = typ; e = expr; CPAREN                                                                                                 { Right (e, t)                   }
  | OPAREN; CASE; e1 = expr; OPAREN; OPAREN; x = VAR; CPAREN; e2 = expr; CPAREN; OPAREN; OPAREN; y = VAR; CPAREN; e3 = expr; CPAREN; CPAREN  { MatchEither (e1, x, e2, y, e3) }
  | OPAREN; e = expr; CPAREN                                                                                                                 { e                              }
  | x = VAR                                                                                                                                  { Id x                           }

expr_list:
 | e = expr; es = expr_list {e :: es}
 | {[]}


typ:
   | INTTY                                      { Int             }
   | BOOL                                       { Bool            }
   | ATOM                                       { Atom            }
   | OPAREN; TIMES; t1 = typ; t2 = typ; CPAREN  { Prod (t1, t2)   }
   | OPAREN; EITHER; t1 = typ; t2 = typ; CPAREN { Either (t1, t2) }
   | OPAREN; ARROW; t1 = typ; t2 = typ; CPAREN  { Arrow (t1, t2)  }
   | OPAREN; LIST; t = typ; CPAREN              { Ast.Lst t       }
   | OPAREN; t = typ; CPAREN                    { t               } 

bop:
  | PLUS  { Plus  }
  | MIN   { Minus }
  | TIMES { Times }
  | LT    { Lt    }
  | EQL   { Eql   }
  | EQ    { Eq    }
  
com:
  | TYPEOF; e = expr       { Ast.Typeof e   }
  | e = expr               { Ast.Eval e     }
  | EVAL; e = expr         { Ast.Eval e     }
  | DEF; x = VAR; e = expr { Ast.Def (x, e) }
  | QUIT                   { Ast.QuitRepl   }