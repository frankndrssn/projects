{
    open Lexing
    open Parser

    exception SyntaxError of string
}

let digit = ['0'-'9']
let int = '-'? digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*

rule read =
    parse
    | white     { read lexbuf                                                           }
    | newline   { new_line lexbuf; read lexbuf                                          }
    | int       { INTLIT (int_of_string (Lexing.lexeme lexbuf))                         }
    | "true"    { TRUE                                                                  }
    | "false"   { FALSE                                                                 }
    | '('       { OPAREN                                                                }
    | ')'       { CPAREN                                                                }
    | ":typeof" { TYPEOF                                                                }
    | ":t"      { TYPEOF                                                                }
    | ":def"    { DEF                                                                   }
    | ":eval"   { EVAL                                                                  }
    | ":q"      { QUIT                                                                  }
    | ":exit"   { QUIT                                                                  }
    | ":quit"   { QUIT                                                                  }
    | ':'       { COLON                                                                 }
    | '\''      { TIC                                                                   } 
    | '`'       { TIC                                                                   }
    | '+'       { PLUS                                                                  }
    | '-'       { MIN                                                                   }
    | '*'       { TIMES                                                                 }
    | '<'       { LT                                                                    }
    | '='       { EQL                                                                   }
    | "eq"      { EQ                                                                    }
    | "if"      { IF                                                                    }
    | "fun"     { FUN                                                                   }
    | "cons"    { CONS                                                                  }
    | "match"   { MATCH                                                                 }
    | '.'       { DOT                                                                   }
    | "fst"     { FST                                                                   }
    | "snd"     { SND                                                                   }
    | "left"    { LEFT                                                                  }
    | "right"   { RIGHT                                                                 }
    | "case"    { CASE                                                                  }
    | "lambda"  { LAMBDA                                                                }
    | "int"     { INTTY                                                                 }
    | "bool"    { BOOL                                                                  }
    | "atom"    { ATOM                                                                  }
    | "either"  { EITHER                                                                }
    | "->"      { ARROW                                                                 }
    | "list"    { LIST                                                                  }
    | id        { VAR (Lexing.lexeme lexbuf)                                            }
    | '['       { OSQ                                                                   }
    | ']'       { CSQ                                                                   }
    | _         { raise (SyntaxError ("Unexpected Character: " ^ Lexing.lexeme lexbuf)) }