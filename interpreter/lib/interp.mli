open Ast


(** A [value] is the result of interpreting an Ast.expr.
    It should contain things like integers, pairs of values, and closures. *)
type value

(** An [env] is an evaluation environment. 
    It maps variables to values.
    It should be immutable and ideally pure. *)
type env

(** A [tenv] is a type environment.
    It maps variables to types.
    It should be immutable and ideally pure. *)
type tenv

(** [string_of_value v] is a human-readable representation of the 
    value [v].
    For closures, it should print the string "#ABSTRACT". 
    For any other type of value, it should print code for the value,
    as in string_of_expr. *)
val string_of_value : value -> string

(** [pretty_value' fmt v] prints [string_of_value v] to [fmt]. *)
val pretty_value' : Format.formatter -> value -> unit

(** [pretty_value v] prints [string_of_value v] to the standard output*)
val pretty_value : value -> unit

(** The empty evaluation environment. 
    It should contain no bindings. *)
val empty_env : env

(** [add_binding x v e] is the environment that results from extending
    [e] by binding [x] to [v].*)
val add_binding : string -> value -> env -> env

(** [lookup_binding x e] is [Some v] if [x] is bound to [v] in [e].
    Returns [None] if there is no such [v]. *)
val lookup_binding : string -> env -> value option

(** The empty type environment.
    It should contain no bindings. *)
val empty_tenv : tenv

(** [add_type_binding x t gamma] is the type environment that results 
    from extending [e] by binding [x] to [t].*)
val add_type_binding : string -> typ -> tenv -> tenv

(** [lookup_type_binding x gamma] is [Some t] if [x] is bound to [t] 
    in [gamma]. Returns [None] if there is no such [v]. *)
val lookup_type_binding : string -> tenv -> typ option

(** [typeof gamma e] computes a type [t] such that gamma |- e : t
    If there is no such type, it returns None. *)
val typeof : tenv -> expr -> typ option

(** [typecheck gamma e t] returns true if gamma |- e : t, 
    and returns false otherwise. *)
val typecheck : tenv -> expr -> typ -> bool

(** [interpret env e] returns [Some v] if computing [e] in 
    environment [env] results in [v].
    If the term gets stuck, it returns [None].
    It should always produce a value for a well-typed term [e]! *)
val interpret : env -> expr -> value option