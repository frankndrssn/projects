type typ = | Int | Bool | Atom | Prod of typ * typ | Either of typ * typ | Arrow of typ * typ | Lst of typ

type bop = Plus | Minus | Times | Lt | Eql | Eq

type expr =
| Id of string
| Lit of int
| Atomic of string
| Binop of bop * expr * expr
| True
| False
| If of expr * expr * expr
| App of expr * expr
| Fun of string * string * typ * typ * expr
| List of typ * expr list
| Cons of expr * expr
| MatchList of expr * expr * string * string * expr
| Pair of expr * expr
| Fst of expr
| Snd of expr
| Left of expr * typ
| Right of expr * typ
| MatchEither of expr * string * expr * string * expr

type command = | Eval of expr | Typeof of expr | QuitRepl | Def of string * expr

let rec pretty_typ' fmt = function
| Int -> Format.fprintf fmt "int"
| Bool -> Format.fprintf fmt "bool"
| Atom -> Format.fprintf fmt "atom"
| Prod (t1, t2) -> Format.fprintf fmt "(*@ @[<2>%a@]@ @[<2>%a@])" pretty_typ' t1 pretty_typ' t2
| Either (t1, t2) -> Format.fprintf fmt "(either@ @[<2>%a@]@ @[<2>%a@])" pretty_typ' t1 pretty_typ' t2
| Arrow (t1, t2) -> Format.fprintf fmt "@[<2>%a@]@ ->@ @[<2>%a@]" pretty_typ' t1 pretty_typ' t2
| Lst t -> Format.fprintf fmt "(list@ @[<2>%a@])" pretty_typ' t

let pretty_binop fmt = function
| Plus -> Format.fprintf fmt "+"
| Minus -> Format.fprintf fmt "-"
| Times -> Format.fprintf fmt "*"
| Lt -> Format.fprintf fmt "<"
| Eql -> Format.fprintf fmt "="
| Eq -> Format.fprintf fmt "eq"

let rec pretty_expr' fmt = function
| Id x -> Format.fprintf fmt "%s" x
| Lit n -> Format.fprintf fmt "%d" n
| Atomic id -> Format.fprintf fmt "`%s" id
| Binop (b,e1,e2) -> Format.fprintf fmt "(@[<1>%a@]@ @[<2>%a@]@ @[<2>%a@])" pretty_binop b pretty_expr' e1 pretty_expr' e2
| True -> Format.fprintf fmt "true"
| False -> Format.fprintf fmt "false"
| If (e1, e2, e3) -> Format.fprintf fmt "(if@ @[<2>%a@]@ @[<2>%a@]@ @[<2>%a@])" pretty_expr' e1 pretty_expr' e2 pretty_expr' e3
| App (e1, e2) -> Format.fprintf fmt "(@[<2>%a@]@ @[<2>%a@])" pretty_expr' e1 pretty_expr' e2
| Fun (f,x,t,s,e) -> Format.fprintf fmt "(fun@ (@[<2>%s@]@ (@[<2>%s : %a@]) : @[<2>%a@])@ @[<4>%a@])" f x pretty_typ' t pretty_typ' s pretty_expr' e
| List (t, es) -> Format.fprintf fmt "`[%a](%a)" pretty_typ' t Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pretty_expr') es
| Cons (e1, e2) -> Format.fprintf fmt "(cons@ @[<2>%a@]@ @[<2>%a@])" pretty_expr' e1 pretty_expr' e2
| MatchList (e1, e2, x, y, e3) ->
  Format.fprintf fmt "(match@ @[<2>%a@]@ @[<2>%a@] @[<2>(@[<4>(@[<6>%s@ %s@])@])@[<4>%a])]" pretty_expr' e1 pretty_expr' e2 x y pretty_expr' e3
| Pair (e1, e2) -> Format.fprintf fmt "`(@[<2>%a@]@ .@ @[<2>%a@])" pretty_expr' e1 pretty_expr' e2
| Fst e -> Format.fprintf fmt "(fst@ @[<2>%a@])" pretty_expr' e
| Snd e -> Format.fprintf fmt "(snd@ @[<2>%a@])" pretty_expr' e
| Left (e, t) -> Format.fprintf fmt "(left@ @[<2>%a@]@ @[<2>%a@])" pretty_expr' e pretty_typ' t
| Right (e, t) -> Format.fprintf fmt "(right@ @[<2>%a@]@ @[<2>%a@])" pretty_typ' t pretty_expr' e
| MatchEither (e1,x,e2,y,e3) -> 
  Format.fprintf fmt "(match@ @[<2>%a@]@ @[<2>(@[<4>(%s)@]@ %a)]@ @[<2>(@[<4>(%s)@]@ %a)@])" pretty_expr' e1 x pretty_expr' e2 y pretty_expr' e3

let pretty_command fmt = function
| Eval e -> Format.fprintf fmt ":eval %a" pretty_expr' e
| Typeof e -> Format.fprintf fmt ":typeof %a" pretty_expr' e
| Def (x, e) -> Format.fprintf fmt ":def %s %a" x pretty_expr' e
| QuitRepl -> Format.fprintf fmt ":q"

let string_of_alpha formatter x =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  formatter fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let string_of_typ = string_of_alpha pretty_typ'

let string_of_expr = string_of_alpha pretty_expr'

let string_of_command = string_of_alpha pretty_command

let pretty_typ = pretty_typ' Format.std_formatter
let pretty_expr = pretty_expr' Format.std_formatter