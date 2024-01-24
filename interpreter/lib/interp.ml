open Ast

let closureString = "#ABSTRACT"


module StringMap = Map.Make(String)
open StringMap

type value =
  | VInt of int
  | VId of string
  | VBool of bool
  | VList of typ * value list
  | VPair of value * value
  | VLeft of value * typ
  | VRight of value * typ
  | VClosure of (env Lazy.t) * string * string * expr
and env = value StringMap.t

type tenv = typ StringMap.t

(** The empty evaluation environment. 
    It should contain no bindings. *)
let empty_env = empty

(** The empty type environment.
    It should contain no bindings. *)
let empty_tenv = empty

(** [add_binding x v e] is the environment that results from extending
    [e] by binding [x] to [v].*)
let add_binding = add

(** [add_type_binding x t gamma] is the type environment that results 
    from extending [e] by binding [x] to [t].*)
let add_type_binding = add

(** [lookup_binding x e] is [Some v] if [x] is bound to [v] in [e].
    Returns [None] if there is no such [v]. *)
let lookup_binding = find_opt

(** [lookup_type_binding x gamma] is [Some t] if [x] is bound to [t] 
    in [gamma]. Returns [None] if there is no such [v]. *)
let lookup_type_binding = find_opt

(** [pretty_value' fmt v] prints [string_of_value v] to [fmt]. *)
let rec pretty_value' fmt = function
  | VInt i -> Format.fprintf fmt "%d" i
  | VId s -> Format.fprintf fmt "`%s" s
  | VBool b -> Format.fprintf fmt (if b then "true" else "false")
  | VList (t, vs) ->
    begin
      Format.fprintf
        fmt
        "`[%a](%a)"
        pretty_typ' t
        Format.(pp_print_list
                  ~pp_sep:(fun out () -> fprintf out "@ ")
                  pretty_value')
        vs
    end
  | VPair (v1, v2) ->
    begin
      Format.fprintf fmt "`(@[<2>%a@]@ .@ @[<2>%a@])"
        pretty_value' v1
        pretty_value' v2
    end
  | VLeft (v, t) ->
    begin
      Format.fprintf fmt "(left@ @[<2>%a@]@ @[<2>%a@])"
        pretty_value' v
        pretty_typ' t
    end
  | VRight (v, t) ->
    begin
      Format.fprintf fmt "(right@ @[<2>%a@]@ @[<2>%a@])"
        pretty_typ' t
        pretty_value' v
    end
  | VClosure _ -> Format.fprintf fmt "%s" closureString

(** [string_of_value v] is a human-readable representation of the 
    value [v].
    For closures, it should print the string "#ABSTRACT". 
    For any other type of value, it should print code for the value,
    as in string_of_expr. *)
let string_of_value = string_of_alpha pretty_value'

(** [pretty_value v] prints [string_of_value v] to the standard output*)
let pretty_value = pretty_value' Format.std_formatter

(** [typeof gamma e] computes a type [t] such that gamma |- e : t
    If there is no such type, it returns None. *)
let rec typeof gamma = function
  | Id x -> lookup_type_binding x gamma
  | Lit _ -> Some Int
  | Atomic _ -> Some Atom
  | Binop (bop, e1, e2) ->
    begin
      match bop with
      | Plus
      | Minus
      | Times ->
        begin
          if typecheck gamma e1 Int && typecheck gamma e2 Int then
            Some Int
          else
            None
        end
      | Lt
      | Eql ->
        begin
          if typecheck gamma e1 Int && typecheck gamma e2 Int then
            Some Bool
          else
            None
        end
      | Eq ->
        begin
          if typecheck gamma e1 Atom && typecheck gamma e2 Atom then
            Some Bool
          else
            None
        end
    end
  | True -> Some Bool
  | False -> Some Bool
  | If (e1, e2, e3) ->
    begin
      match (typeof gamma e2, typeof gamma e3) with
      | (Some t1, Some t2) ->
        begin
          if typecheck gamma e1 Bool && t1 = t2 then
            Some t1
          else
            None
        end
      | _ -> None
    end
  | App (e1, e2) ->
    begin
      (* I guess we can just synthesize e1's type *)
      match typeof gamma e1 with
      | Some (Arrow (t1, t2)) ->
        begin
          if typecheck gamma e2 t1 then
            Some t2
          else
            None
        end
      | _ -> None
    end
  | Fun (f, x, t1, t2, e) ->
    begin
      (* Just needa typecheck e in this case *)
      let gamma' = add_type_binding
          f
          (Arrow (t1, t2))
          (add_type_binding x t1 gamma)
      in
      if typecheck gamma' e t2 then
        Some (Arrow (t1, t2))
      else
        None
    end
  | List (t, es) ->
    begin
      if List.for_all (fun e -> typecheck gamma e t) es then
        Some (Lst t)
      else
        None
    end
  | Cons (e1, e2) ->
    begin
      match typeof gamma e2 with
      | Some (Lst t) ->
        begin
          if typecheck gamma e1 t then
            Some (Lst t)
          else
            None
        end
      | _ -> None
    end
  | MatchList (e1, e2, x, y, e3) ->
    begin
      match (typeof gamma e1, typeof gamma e2) with
      | (Some (Lst t1), Some t2) ->
        begin
          let gamma' = add_type_binding
              x
              t1
              (add_type_binding y (Lst t1) gamma)
          in
          if typecheck gamma' e3 t2 then
            Some t2
          else
            None
        end
      | _ -> None
    end
  | Pair (e1, e2) ->
    begin
      match (typeof gamma e1, typeof gamma e2) with
      | (Some t1, Some t2) -> Some (Prod (t1, t2))
      | _ -> None
    end
  | Fst e ->
    begin
      match typeof gamma e with
      | Some (Prod (t1, _)) -> Some t1
      | _ -> None
    end
  | Snd e ->
    begin
      match typeof gamma e with
      | Some (Prod (_, t2)) -> Some t2
      | _ -> None
    end
  | Left (e, t) ->
    begin
      match typeof gamma e with
      | Some tl -> Some (Either (tl, t))
      | None -> None
    end
  | Right (e, t) ->
    begin
      match typeof gamma e with
      | Some tr -> Some (Either (t, tr))
      | None -> None
    end
  | MatchEither (e1, x, e2, y, e3) ->
    begin
      match typeof gamma e1 with
      | Some (Either (t1, t2)) ->
        begin
          let r1 = typeof (add_type_binding x t1 gamma) e2 in
          let r2 = typeof (add_type_binding y t2 gamma) e3 in
          match (r1, r2) with
          | (Some r1, Some r2) ->
            begin
              if r1 = r2 then
                Some r1
              else
                None
            end
          | _ -> None
        end
      | _ -> None
    end
and typecheck gamma expr typ =
  match typeof gamma expr with
  | Some t -> t = typ
  | None -> false
(** [typecheck gamma e t] returns true if gamma |- e : t, 
    and returns false otherwise. *)

(** [interpret env e] returns [Some v] if computing [e] in 
    environment [env] results in [v].
    If the term gets stuck, it returns [None].
    It should always produce a value for a well-typed term [e]! *)
let rec interpret env = function
  | Id x -> lookup_binding x env
  | Lit i -> Some (VInt i)
  | Atomic s -> Some (VId s)
  | Binop (binop, e1, e2) ->
    begin
      match (binop, interpret env e1, interpret env e2) with
      | (Plus, Some (VInt v1), Some (VInt v2)) -> Some (VInt (v1 + v2))
      | (Minus, Some (VInt v1), Some (VInt v2)) -> Some (VInt (v1 - v2))
      | (Times, Some (VInt v1), Some (VInt v2)) -> Some (VInt (v1 * v2))
      | (Lt, Some (VInt v1), Some (VInt v2)) -> Some (VBool (v1 < v2))
      | (Eql, Some (VInt v1), Some (VInt v2)) -> Some (VBool (v1 = v2))
      | (Eq, Some (VId v1), Some (VId v2)) -> Some (VBool (v1 = v2))
      | _ -> None
    end
  | True -> Some (VBool true)
  | False -> Some (VBool false)
  | If (e1, e2, e3) ->
    begin
      match interpret env e1 with
      | Some (VBool b) ->
        begin
          interpret env (if b then e2 else e3)
        end
      | _ -> None
    end
  | App (e1, e2) ->
    begin
      match (interpret env e1, interpret env e2) with
      | (Some (VClosure (env_thunk, f, x, e) as cl), Some v) ->
        begin
          let env' = add_binding
              f
              cl
              (add_binding x v (Lazy.force env_thunk))
          in
          interpret env' e
        end
      | _ -> None
    end
  | Fun (f, x, _, _, e) ->
    begin
      Some (VClosure (lazy env, f, x, e))
    end
  | List (t, es) ->
    begin
      let vs_opt = List.fold_left (fun acc e ->
          match (acc, interpret env e) with
          | (Some vs, Some v) -> Some (v :: vs)
          | _ -> None)
          (Some [])
          es
      in
      match vs_opt with
      (* Need to reverse the list because of how fold_left works *)
      | Some vs -> Some (VList (t, List.rev vs))
      | None -> None
    end
  | Cons (e1, e2) ->
    begin
      match (interpret env e1, interpret env e2) with
      | (Some v, Some (VList (t, vs))) -> Some (VList (t, v :: vs))
      | _ -> None
    end
  | MatchList (e1, e2, x, y, e3) ->
    begin
      match interpret env e1 with
      | Some (VList (_, [])) -> interpret env e2
      | Some (VList (t, v1 :: vs)) ->
        begin
          let env' = add_binding
              x
              v1
              (add_binding y (VList (t, vs)) env)
          in
          interpret env' e3
        end
      | _ -> None
    end
  | Pair (e1, e2) ->
    begin
      match (interpret env e1, interpret env e2) with
      | (Some v1, Some v2) -> Some (VPair (v1, v2))
      | _ -> None
    end
  | Fst e ->
    begin
      match interpret env e with
      | Some (VPair (v1, _)) -> Some v1
      | _ -> None
    end
  | Snd e ->
    begin
      match interpret env e with
      | Some (VPair (_, v2)) -> Some v2
      | _ -> None
    end
  | Left (e, t) ->
    begin
      match interpret env e with
      | Some v -> Some (VLeft (v, t))
      | _ -> None
    end
  | Right (e, t) ->
    begin
      match interpret env e with
      | Some v -> Some (VRight (v, t))
      | _ -> None
    end
  | MatchEither (e1, x, e2, y, e3) ->
    begin
      match interpret env e1 with
      | Some (VLeft (v, _)) -> interpret (add_binding x v env) e2
      | Some (VRight (v, _)) -> interpret (add_binding y v env) e3
      | _ -> None
    end
