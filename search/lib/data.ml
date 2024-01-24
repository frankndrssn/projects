exception Unimplemented

module type Formattable = sig
  type t
  val format : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> [ `EQ | `GT | `LT ]
  include Formattable with type t := t
end

type ('k,'v) tree23 =
  | Leaf
  | Twonode of ('k,'v) twonode
  | Threenode of ('k,'v) threenode
and ('k,'v) twonode = {
  left2  : ('k,'v) tree23;
  value  : 'k * 'v;
  right2 : ('k,'v) tree23;
}
and ('k,'v) threenode = {
  left3   : ('k,'v) tree23;
  lvalue  : 'k * 'v;
  middle3 : ('k,'v) tree23;
  rvalue  : 'k * 'v;
  right3  : ('k,'v) tree23;
}

module type Dictionary = sig
  module Key : Comparable
  module Value : Formattable
  type key = Key.t
  type value = Value.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : key -> value -> t -> t
  val member : key -> t -> bool
  val find : key -> t -> value option
  val remove : key -> t -> t
  val choose : t -> (key * value) option
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> (key * value) list
  val expose_tree : t -> (key,value) tree23
  val format : Format.formatter -> t -> unit
end

module type DictionaryMaker =
  functor (K : Comparable) (V : Formattable)
    -> Dictionary with module Key = K and module Value = V

module MakeListDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t


  (*
     AF: A dictionary is represented as a list of key-value pairs.
     - Examples:
       + [] represents the empty dictionary.
       + [(k1,v1),(k2,v2)] represents a dictionary where [k1] is bound to [v1],
         and [k2] is bound to [v2].
     RI: Duplicate keys are not allowed.
  *)
  type t = (key * value) list
  let debug = false

  (* [rep_ok d] returns [d] if [d] satisfies its representation
     * invariants. It's unusual for a data abstraction to
     * expose this function to its clients, but we do so here
     * to ensure that you implement it.
     * raises: [Failure] with an unspecified error message
     *   if [d] does not satisfy its representation invariants. *)
  let rep_ok d =
    let rec go k = function
      | [] -> `OK
      | (k',_) :: xs -> begin
          match K.compare k k' with
          | `EQ -> `NOTOK
          | _ -> go k xs
        end
    in
    let rec check = function
      | [] -> `OK
      | (k,_) :: xs -> begin
          match go k xs with
          | `OK -> check xs
          | `NOTOK -> `NOTOK
        end
    in
    if debug then
      match check d with
      | `OK -> d
      | `NOTOK -> failwith "RI"
    else
      d

  (* [empty] is the empty dictionary *)
  let empty = []

  (* [is_empty d] is [true] iff [d] is empty. *)
  let is_empty d =
    match rep_ok d with
    | [] -> true
    | _ -> false

  (* [size d] is the number of bindings in [d]. *
   * [size empty] is [0]. *)
  let size d = List.length (rep_ok d)

  (* [remove k d] contains all the bindings of [d] except
   * a binding for [k].  If [k] is not bound in [d], then
   * [remove] returns a dictionary with the same bindings
   * as [d]. *)
  let remove k d =
    let neq_k (k',_) =
      match K.compare k k' with
      | `EQ -> false
      | _ -> true
    in
    List.filter neq_k (rep_ok d)

  (* [insert k v d] is [d] with [k] bound to [v]. If [k] was already
   * bound, its previous value is replaced with [v]. *)
  let insert k v d =
    let res = (k,v) :: remove k (rep_ok d) in
    rep_ok res

  (**
     [eq_k k (k',_)] is true iff [k] equal [k'] based on [K.compare].
  *)
  let eq_k k (k',_) =
    match K.compare k k' with
    | `EQ -> true
    | _ -> false

  (* [find k d] is [Some v] if [k] is bound to [v] in [d]; or
   * if [k] is not bound, then it is [None]. *)
  let find k d =
    Option.bind (rep_ok d
                 |> List.find_opt (eq_k k))
      (fun (_,v) -> Some v)

  (* [member k d] is [true] iff [k] is bound in [d]. *)
  let member k d = List.exists (eq_k k) (rep_ok d)

  (* [choose d] is [Some (k,v)], where [k] is bound to [v]
   * in [d].  It is unspecified which binding of [d] is
   * returned.  If [d] is empty, then [choose d] is [None]. *)
  let choose = function
    | [] -> None
    | (k,v) :: _ -> Some (k,v)

  (**
     [sort d] is equivalent to [d] but keys are presented in increasing order
     based on [K.compare].
  *)
  let sort =
    let compare (k1,_) (k2,_) =
      match K.compare k1 k2 with
      | `EQ -> 0
      | `LT -> -1
      | `GT -> 1
    in
    List.sort compare

  (* [to_list d] is an association list containing the same
   * bindings as [d].  The order of elements in the list is
   * in order from the least key to the greatest. *)
  let to_list d = sort (rep_ok d)

  (* [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)],
   * if [d] binds [ki] to [vi].  Bindings are processed
   * in order from least to greatest, where [k1] is the
   * least key and [kn] is the greatest. *)
  let fold f init d =
    let d_sorted = sort (rep_ok d) in
    (* Type checker needs some help. *)
    List.fold_left
      (fun acc ((k,v) : key * value) -> f k v acc)
      init
      d_sorted

  (* [expose_tree d] is the 2-3 tree representing [d].  It's unusual
   * for a data abstraction to expose its representation like this,
   * but we do it for testing purposes, as described above.
   * raises: an unspecified exception if [d] was not created through
   *   [MakeTreeDictionary]. *)
  let expose_tree d =
    failwith "not a 2-3 tree"

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of a dictionary
   * on the given formatter. *)
  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module MakeTreeDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t


  (**
     AF: Each node contains a key-value pair representing a key-value binding.
         Leaf represents the dictionary with no bindings.
     RI:
     - Given a node, every subtree of that node has the same height.
     - Given a 2-node r, every key in the left subtree is less than
       the key in r, and every key in the right subtree is larger
     - Given a 3-node r,
       + the left key is less than the right key
       + every key in the left subtree is less than the
         left key in r
       + every key in the middle subtree is between
       + every key in the right subtree is larger
  *)

  type t = (key,value) tree23


  (* [rep_ok d] returns [d] if [d] satisfies its representation
   * invariants. It's unusual for a data abstraction to
   * expose this function to its clients, but we do so here
   * to ensure that you implement it.
   * raises: [Failure] with an unspecified error message
   *   if [d] does not satisfy its representation invariants. *)
  let rep_ok d =
    let get_key = function
      | Leaf -> None
      | Twonode {value = (k,_)} -> Some (`Two k)
      | Threenode {lvalue = (kl, _); rvalue = (kr, _)} -> Some (`Three (kl, kr))
    in
    let check_less k = function
      | None -> ()
      | Some (`Two k') ->
        begin
          match K.compare k k' with
          | `LT -> ()
          | _ -> failwith "RI"
        end
      | Some (`Three (kl', kr')) ->
        begin
          match (K.compare k kl', K.compare k kr') with
          | (`LT, `LT) -> ()
          | _ -> failwith "RI"
        end
    in
    let check_greater k = function
      | None -> ()
      | Some (`Two k') ->
        begin
          match K.compare k k' with
          | `GT -> ()
          | _ -> failwith "RI"
        end
      | Some (`Three (kl', kr')) ->
        begin
          match (K.compare k kl', K.compare k kr') with
          | (`GT, `GT) -> ()
          | _ -> failwith "RI"
        end
    in
    match d with
    | Leaf -> Leaf
    | Twonode ({value = (k,_)} as n) ->
      begin
        check_greater k (get_key n.left2);
        check_less k (get_key n.right2);
        Twonode n
      end
    | Threenode ({lvalue = (kl, _); rvalue = (kr, _)} as n) ->
      begin
        check_greater kl (get_key n.left3);
        check_less kl (get_key n.middle3);
        check_greater kr (get_key n.middle3);
        check_less kr (get_key n.right3);
        Threenode n
      end

  (* [empty] is the empty dictionary *)
  let empty = Leaf

  (* [is_empty d] is [true] iff [d] is empty. *)
  let is_empty = function
    | Leaf -> true
    | _ -> false

  (* [size d] is the number of bindings in [d]. *
   * [size empty] is [0]. *)
  let rec size = function
    | Leaf -> 0
    | Twonode {left2 = l; right2 = r} ->
      1 + size l + size r
    | Threenode {left3 = l; middle3 = m; right3 = r} ->
      2 + size l + size m + size r



  (* A representation for the kickup configuration *)
  (* This keeps track of the upward phase so my brain doesn't explode. *)
  type kickup =
    | KTwol of (upor23 * (key * value) * t)
    | KTwor of (t * (key * value) * upor23)
    | KThreel of (upor23 * (key * value) * t * (key * value) * t)
    | KThreem of (t * (key * value) * upor23 * (key * value) * t)
    | KThreer of (t * (key * value) * t * (key * value) * upor23)
  and up = t * (key * value) * t
  and upor23 =
    | Up of up
    | K23 of t

  (**
     [insert_up kickup] is the upward phase detailed in the handout.
  *)
  let insert_up = function
    | KTwol (K23 l, x, r)
    | KTwor (l, x, K23 r) ->
      begin
        K23 (Twonode {
            left2 = l;
            value = x;
            right2 = r;
          })
      end
    | KThreel (K23 l, x, m, y, r)
    | KThreem (l, x, K23 m, y, r)
    | KThreer (l, x, m, y, K23 r) ->
      begin
        K23 (Threenode {
            left3 = l;
            lvalue = x;
            middle3 = m;
            rvalue = y;
            right3 = r;
          })
      end
    | KTwol (Up (l, w, m), x, r)
    | KTwor (l, w, Up (m, x, r)) ->
      begin
        K23 (Threenode {
            left3 = l;
            lvalue = w;
            middle3 = m;
            rvalue = x;
            right3 = r;
          })
      end
    | KThreel (Up (a, w, b), x, c, y, d)
    | KThreem (a, w, Up (b, x, c), y, d)
    | KThreer (a, w, b, x, Up (c, y, d)) ->
      begin
        Up (Twonode {left2 = a; value = w; right2 = b},
            x,
            Twonode {left2 = c; value = y; right2 = d})
      end

  (**
     [insert_down v t] is the downward phase detailed in the handout.
  *)
  let rec insert_down (k,v) = function
    | Leaf -> Up (Leaf, (k,v), Leaf)
    | Twonode ({left2 = l; right2 = r} as n) ->
      begin
        let (nk, _) = n.value in
        match K.compare k nk with
        (* equal keys -> shadow *)
        | `EQ -> K23 (Twonode {n with value = (k,v)})
        (* < means go left *)
        | `LT -> insert_up (KTwol (insert_down (k,v) l, n.value, r))
        | `GT -> insert_up (KTwor (l, n.value, insert_down (k,v) r))
      end
    | Threenode ({left3 = l; middle3 = m; right3 = r} as n) ->
      begin
        let (nkl, _) = n.lvalue in
        let (nkr, _) = n.rvalue in
        match K.compare k nkl with
        | `EQ -> K23 (Threenode {n with lvalue = (k,v)})
        | `LT ->
          begin
            insert_up (KThreel (insert_down (k,v) l,
                                n.lvalue,
                                m,
                                n.rvalue,
                                r))
          end
        | `GT ->
          begin
            match K.compare k nkr with
            | `EQ -> K23 (Threenode {n with rvalue = (k,v)})
            | `LT ->
              begin
                insert_up (KThreem (l,
                                    n.lvalue,
                                    insert_down (k,v) m,
                                    n.rvalue,
                                    r))
              end
            | `GT ->
              begin
                insert_up (KThreer (l,
                                    n.lvalue,
                                    m,
                                    n.rvalue,
                                    insert_down (k,v) r))
              end
          end
      end

  (* [insert k v d] is [d] with [k] bound to [v]. If [k] was already
   * bound, its previous value is replaced with [v]. *)
  let insert k v d =
    match insert_down (k,v) d with
    | K23 t -> t
    | Up (l, x, r) -> Twonode {left2 = l; value = x; right2 = r}



  (*
     A type that keeps track of those remove cases in the handout.
     This makes my brain explode.
  *)
  type tree_with_hole =
    | HTwol of hole_or_23 * (key * value) * t
    | HTwor of t * (key * value) * hole_or_23
    | HThreel of hole_or_23 * (key * value) * t * (key * value) * t
    | HThreem of t * (key * value) * hole_or_23 * (key * value) * t
    | HThreer of t * (key * value) * t * (key * value) * hole_or_23
  and hole_or_23 =
    | Hole of t
    | H23 of t

  (* It's easy to remove a leaf node so we're turning the problem of removing
     an arbitrary node into that of removing a leaf node. *)

  (**
     [leftmost t] is the leftmost key-value pair of [t].
     If [t] is the right subtree of some root node r = (k,_),
     then the key of [leftmost t] is the inorder successor of k.
  *)
  let rec leftmost = function
    | Leaf -> None
    | Twonode {left2 = Leaf; value = x}
    | Threenode {left3 = Leaf; lvalue = x} -> Some x
    | Twonode {left2 = l}
    | Threenode {left3 = l} -> leftmost l

  (**
     [rightmost t] is the rightmost key-value pair of [t].
  *)
  let rec rightmost = function
    | Leaf -> None
    | Twonode {value = x; right2 = Leaf}
    | Threenode {rvalue = x; right3 = Leaf} -> Some x
    | Twonode {right2 = r}
    | Threenode {right3 = r} -> rightmost r

  (**
     [remove_up h] is the upward phase detailed in the handout.
  *)
  let remove_up = function
    | HTwol (Hole l, x, Twonode {left2 = m; value = y; right2 = r})
    | HTwor (Twonode {left2 = l; value = x; right2 = m}, y, Hole r) ->
      begin
        Hole (Threenode {
            left3 = l;
            lvalue = x;
            middle3 = m;
            rvalue = y;
            right3 = r;
          })
      end
    | HTwol (Hole a, x, Threenode {
        left3 = b;
        lvalue = y;
        middle3 = c;
        rvalue = z;
        right3 = d})
    | HTwor (Threenode {
          left3 = a;
          lvalue = x;
          middle3 = b;
          rvalue = y;
          right3 = c
        }, z, Hole d) ->
      begin
        H23 (Twonode {
            left2 = Twonode {left2 = a; value = x; right2 = b};
            value = y;
            right2 = Twonode {left2 = c; value = z; right2 = d};
          })
      end
    | HThreel (Hole a, x, Twonode {left2 = b; value = y; right2 = c}, z, d)
    | HThreem (Twonode {left2 = a; value = x; right2 = b}, y, Hole c, z, d) ->
      begin
        H23 (Twonode {
            left2 = Threenode {
                left3 = a;
                lvalue = x;
                middle3 = b;
                rvalue = y;
                right3 = c};
            value = z;
            right2 = d
          })
      end
    | HThreem (a, x, Hole b, y, Twonode {left2 = c; value = z; right2 = d})
    | HThreer (a, x, Twonode {left2 = b; value = y; right2 = c}, z, Hole d) ->
      begin
        H23 (Twonode {
            left2 = a;
            value = x;
            right2 = Threenode {
                left3 = b;
                lvalue = y;
                middle3 = c;
                rvalue = z;
                right3 = d;
              }
          })
      end
    | HThreel (Hole a, w, Threenode {
        left3 = b;
        lvalue = x;
        middle3 = c;
        rvalue = y;
        right3 = d}, z, e)
    | HThreem (Threenode {
          left3 = a;
          lvalue = w;
          middle3 = b;
          rvalue = x;
          right3 = c;
        }, y, Hole d, z, e) ->
      begin
        H23 (Threenode {
            left3 = Twonode {left2 = a; value = w; right2 = b};
            lvalue = x;
            middle3 = Twonode {left2 = c; value = y; right2 = d};
            rvalue = z;
            right3 = e;
          })
      end
    | HThreem (a, w, Hole b, x, Threenode {
        left3 = c;
        lvalue = y;
        middle3 = d;
        rvalue = z;
        right3 = e})
    | HThreer (a, w, Threenode {
          left3 = b;
          lvalue = x;
          middle3 = c;
          rvalue = y;
          right3 = d;
        }, z, Hole e) ->
      begin
        H23 (Threenode {
            left3 = a;
            lvalue = w;
            middle3 = Twonode {left2 = b; value = x; right2 = c};
            rvalue = y;
            right3 = Twonode {left2 = d; value = z; right2 = e}
          })
      end
    | HTwol (H23 l, x, r)
    | HTwor (l, x, H23 r) ->
      begin
        H23 (Twonode {left2 = l; value = x; right2 = r})
      end
    | HThreel (H23 l, x, m, y, r)
    | HThreem (l, x, H23 m, y, r)
    | HThreer (l, x, m, y, H23 r) ->
      begin
        H23 (Threenode {
            left3 = l;
            lvalue = x;
            middle3 = m;
            rvalue = y;
            right3 = r})
      end
    | _ -> failwith "RI"
  (* These are the cases matched by _: *)
  (* | HTwol (Hole _, _, Leaf) *)
  (* | HTwor (Leaf, _, Hole _) *)
  (* | HThreel (Hole _, _, Leaf, _, _) *)
  (* | HThreem (Leaf, _, Hole _, _, Leaf) *)
  (* | HThreer (_, _, Leaf, _, Hole _) -> failwith "RI" *)

  (**
     [remove_leaf k t] removes a leaf node in [t] whose key is [k].
  *)
  let rec remove_leaf k = function
    | Leaf -> H23 Leaf
    | Twonode ({left2 = Leaf; right2 = Leaf} as n) ->
      begin
        let (nk, _) = n.value in
        match K.compare k nk with
        | `EQ -> Hole Leaf
        | _ -> H23 (Twonode n)
      end
    | Threenode ({left3 = Leaf; middle3 = Leaf; right3 = Leaf} as n) ->
      begin
        let (nkl, _) = n.lvalue in
        let (nkr, _) = n.rvalue in
        match K.compare k nkl with
        | `EQ -> H23 (Twonode {left2 = Leaf; value = n.rvalue; right2 = Leaf})
        | _ ->
          begin
            match K.compare k nkr with
            | `EQ -> H23 (Twonode {left2 = Leaf;
                                   value = n.lvalue;
                                   right2 = Leaf})
            | _ -> H23 (Threenode n)
          end
      end
    | Twonode ({value = (nk,_)} as n) ->
      begin
        remove_up @@
        match K.compare k nk with
        | `LT -> HTwol (remove_leaf k n.left2, n.value, n.right2)
        | _ -> HTwor (n.left2, n.value, remove_leaf k n.right2)
      end
    | Threenode ({lvalue = (nkl, _); rvalue = (nkr, _)} as n) ->
      begin
        remove_up @@
        match K.compare k nkl with
        | `LT -> HThreel (remove_leaf k n.left3,
                          n.lvalue,
                          n.middle3,
                          n.rvalue,
                          n.right3)
        | _ ->
          begin
            match K.compare k nkr with
            | `LT -> HThreem (n.left3,
                              n.lvalue,
                              remove_leaf k n.middle3,
                              n.rvalue,
                              n.right3)
            | _ -> HThreer (n.left3,
                            n.lvalue,
                            n.middle3,
                            n.rvalue,
                            remove_leaf k n.right3)
          end
      end


  (* TODO: refactor the following three functions. *)


  (**
     [remove_two_node t] removes the given 2-node.
     Requires:
     - [t] is a 2-node.
  *)
  let remove_two_node = function
    | Twonode n ->
      begin
        match rightmost n.left2 with
        | Some (pred, v) ->
          begin
            remove_up @@ HTwol (remove_leaf pred n.left2, (pred,v), n.right2)
          end
        | None ->
          begin
            match leftmost n.right2 with
            | None -> Hole Leaf
            | Some (suc,v) ->
              begin
                remove_up @@ HTwor (n.left2, (suc,v), remove_leaf suc n.right2)
              end
          end
      end
    | _  -> failwith "bad"

  (**
     [remove_three_node_l t] removes the left value of the given 3-node.
     Requires:
     - [t] is a 3-node.
  *)
  let remove_three_node_l = function
    | Threenode n ->
      begin
        match rightmost n.left3 with
        | Some (pred,v) ->
          begin
            remove_up @@ HThreel (remove_leaf pred n.left3,
                                  (pred,v),
                                  n.middle3,
                                  n.rvalue,
                                  n.right3)
          end
        | None ->
          begin
            match leftmost n.middle3 with
            | None -> H23 (Twonode {left2 = Leaf;
                                    value = n.rvalue;
                                    right2 = n.right3})
            | Some (suc,v) ->
              begin
                remove_up @@ HThreem (n.left3,
                                      (suc,v),
                                      remove_leaf suc n.middle3,
                                      n.rvalue,
                                      n.right3)
              end
          end
      end
    | _ -> failwith "bad"

  (**
     [remove_three_node_r t] removes the right value of the given 3-node.
     Requires:
     - [t] is a 3-node.
  *)
  let remove_three_node_r = function
    | Threenode n ->
      begin
        match rightmost n.middle3 with
        | Some (pred,v) ->
          begin
            remove_up @@ HThreem (n.left3,
                                  n.lvalue,
                                  remove_leaf pred n.middle3,
                                  (pred,v),
                                  n.right3)
          end
        | None ->
          begin
            match leftmost n.right3 with
            | None -> H23 (Twonode {left2 = n.left3;
                                    value = n.lvalue;
                                    right2 = Leaf})
            | Some (suc,v) ->
              begin
                remove_up @@ HThreer (n.left3,
                                      n.lvalue,
                                      n.middle3,
                                      (suc,v),
                                      remove_leaf suc n.right3)
              end
          end
      end
    | _ -> failwith "bad"

  (**
     [remove_down k t] is the downward phase briefly mentioned in the
     handout.
  *)
  let rec remove_down k = function
    | Leaf -> H23 Leaf
    | Twonode n ->
      begin
        let (nk, _) = n.value in
        match K.compare k nk with
        | `EQ -> remove_two_node (Twonode n)
        | `LT -> remove_up @@ HTwol (remove_down k n.left2, n.value, n.right2)
        | `GT -> remove_up @@ HTwor (n.left2, n.value, remove_down k n.right2)
      end
    | Threenode n ->
      begin
        let (nkl, _) = n.lvalue in
        let (nkr, _) = n.rvalue in
        match K.compare k nkl with
        | `EQ -> remove_three_node_l (Threenode n)
        | `LT -> remove_up @@ HThreel (remove_down k n.left3,
                                       n.lvalue,
                                       n.middle3,
                                       n.rvalue,
                                       n.right3)
        | `GT ->
          begin
            match K.compare k nkr with
            | `EQ -> remove_three_node_r (Threenode n)
            | `LT -> remove_up @@ HThreem (n.left3,
                                           n.lvalue,
                                           remove_down k n.middle3,
                                           n.rvalue,
                                           n.right3)
            | `GT -> remove_up @@ HThreer (n.left3,
                                           n.lvalue,
                                           n.middle3,
                                           n.rvalue,
                                           remove_down k n.right3)
          end
      end

  (* [remove k d] contains all the bindings of [d] except
     * a binding for [k].  If [k] is not bound in [d], then
     * [remove] returns a dictionary with the same bindings
     * as [d]. *)
  let remove k d =
    match remove_down k d with
    | Hole t
    | H23 t -> t

  (* [find k d] is [Some v] if [k] is bound to [v] in [d]; or
   * if [k] is not bound, then it is [None]. *)
  let rec find k = function
    | Leaf -> None
    | Twonode ({left2 = l; right2 = r} as n) ->
      begin
        let (nk, v) = n.value in
        match K.compare k nk with
        | `EQ -> Some v
        | `LT -> find k l
        | `GT -> find k r
      end
    | Threenode ({left3 = l; middle3 = m; right3 = r} as n) ->
      begin
        let (nkl, vl) = n.lvalue in
        let (nkr, vr) = n.rvalue in
        match K.compare k nkl with
        | `EQ -> Some vl
        | `LT -> find k l
        | `GT ->
          begin
            match K.compare k nkr with
            | `EQ -> Some vr
            | `LT -> find k m
            | `GT -> find k r
          end
      end

  (* [member k d] is [true] iff [k] is bound in [d]. *)
  let rec member k = function
    | Leaf -> false
    | Twonode ({left2 = l; right2 = r} as n) ->
      begin
        let (nk, _) = n.value in
        match K.compare k nk with
        | `EQ -> true
        | `LT -> member k l
        | `GT -> member k r
      end
    | Threenode ({left3 = l; middle3 = m; right3 = r} as n) ->
      begin
        let (nkl, _) = n.lvalue in
        let (nkr, _) = n.rvalue in
        match K.compare k nkl with
        | `EQ -> true
        | `LT -> member k l
        | `GT ->
          begin
            match K.compare k nkr with
            | `EQ -> true
            | `LT -> member k m
            | `GT -> member k r
          end
      end

  (* [choose d] is [Some (k,v)], where [k] is bound to [v]
   * in [d].  It is unspecified which binding of [d] is
   * returned.  If [d] is empty, then [choose d] is [None]. *)
  let choose = function
    | Leaf -> None
    | Twonode {value = kv}
    | Threenode {lvalue = kv} -> Some kv

  (* [to_list d] is an association list containing the same
   * bindings as [d].  The order of elements in the list is
   * in order from the least key to the greatest. *)
  let rec to_list = function
    | Leaf -> []
    | Twonode ({left2 = l; right2 = r} as n) ->
      begin
        to_list l @ [n.value] @ to_list r
      end
    | Threenode ({left3 = l; middle3 = m; right3 = r} as n) ->
      begin
        to_list l @ [n.lvalue] @ to_list m @ [n.rvalue] @ to_list r
      end

  (* [expose_tree d] is the 2-3 tree representing [d].  It's unusual
   * for a data abstraction to expose its representation like this,
   * but we do it for testing purposes, as described above.
   * raises: an unspecified exception if [d] was not created through
   *   [MakeTreeDictionary]. *)
  let expose_tree d =
    d

  (* [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)],
   * if [d] binds [ki] to [vi].  Bindings are processed
   * in order from least to greatest, where [k1] is the
   * least key and [kn] is the greatest. *)
  let rec fold f init = function
    | Leaf -> init
    | Twonode {left2 = l; value = (k, v); right2 = r} ->
      begin
        fold f (f k v (fold f init l)) r
      end
    | Threenode {left3 = l;
                 lvalue = (kl, vl);
                 middle3 = m;
                 rvalue = (kr, vr);
                 right3 = r} ->
      begin
        fold f (f kr vr (fold f (f kl vl (fold f init l)) m)) r
      end

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of a dictionary
   * on the given formatter. *)
  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module type Set = sig
  module Elt : Comparable
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module MakeSetOfDictionary (C : Comparable) (DM:DictionaryMaker) = struct
  module Elt = C
  type elt = Elt.t
  (* Just expose it... *)
  module Token : Formattable with type t = unit = struct
    type t = unit
    let format _ _ = ()
  end


  (*
     AF: A set is represented as a dictionary of key-value bindings, which
     can be thought of as (element, structure). Since elements of a set do not
     have any underlying structure, the values are units.
     Examples:
     - The empty dictionary represents the empty set.
     - The dictionary {(k1,_),(k2,_),(k3,_)} represents a set containing the
       elements k1, k2, and k3.
     RI: None.
  *)
  module Dict = DM(Elt)(Token)
  type t = Dict.t

  let rep_ok s = s

  let empty = Dict.empty

  let is_empty = Dict.is_empty

  let size = Dict.size

  let insert e s = Dict.insert e () s

  let member = Dict.member

  let remove = Dict.remove

  let choose s = Option.bind (Dict.choose s) (fun (pr1,_) -> Some pr1)

  let fold f init s = Dict.fold (fun e _ acc -> f e acc) init s

  let to_list s =
    Dict.to_list s
    |> List.map (fun (pr1,_) -> pr1)

  let union s1 s2 =
    List.fold_left (fun acc e -> insert e acc) s1 (to_list s2)

  let intersect s1 s2 =
    List.fold_left
      (fun acc e ->
         if member e s1 then
           insert e acc
         else
           acc)
      empty
      (to_list s2)

  let difference s1 s2 =
    List.fold_left
      (fun acc e ->
         if member e s1 then
           remove e acc
         else
           acc)
      s1
      (to_list s2)

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end
