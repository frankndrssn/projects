exception Unimplemented

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end


module MakeEngine
    (S:Data.Set with type Elt.t = string)
    (D:Data.Dictionary with type Key.t = string and type Value.t = S.t)
  : Engine
=
struct
  type idx = D.t

  let ( >>= ) = Option.bind


  (**
     [txt_in_dir dname] is a list of .txt files in [dname].
     Raises:
     - [Unix_error] when system calls return an error.
  *)
  let txt_in_dir dname =
    let open Unix in
    let open Str in
    let regex = regexp {|^.*\.txt$|} in
    let dname =
      dname ^
      if Filename.check_suffix dname (Filename.dir_sep) then
        ""
      else
        Filename.dir_sep
    in
    let rec go acc handle =
      try
        let fn = readdir handle in
        go ((dname ^ fn) :: acc) handle
      with
      | _ -> closedir handle; acc
    in
    go [] (opendir dname)
    |> List.filter (fun s -> string_match regex s 0)

  (**
     [sanitize ss] removes empty strings in [ss]
  *)
  let sanitize = List.filter (fun s -> s <> "")

  (**
     [sf regex s] is the same as [Str.search_forward regex s 0], but returns
     an option.
  *)
  let sf regex s =
    try
      Some (Str.search_forward regex s 0)
    with
    | _ -> None


  (**
     [sb regex s] is the same as
     [Str.search_backward regex s (String.length s)], but returns an option.
  *)
  let sb regex s =
    try
      Some (Str.search_backward regex s (String.length s))
    with
    | _ -> None

  (**
     [prewords line] is the list of prewords in [line].
  *)
  let prewords line =
    let open Str in
    let regex = regexp "[ \\|\t\\|\n\\|\r]" in
    sanitize (split regex line)

  (**
     [words prewords] is the list of words in [prewords].
  *)
  let words prewords =
    let to_lcword = function
      | Some w -> String.lowercase_ascii w
      | None -> ""
    in
    let word preword =
      let open Str in
      let regex = regexp {|[A-Za-z0-9]|} in
      to_lcword (sf regex preword
                 >>= (fun i -> Some (string_after preword i))
                 >>= (fun pw' -> sb regex pw'
                       >>= (fun i -> Some (string_before pw' (i+1)))))
    in
    sanitize (List.map word prewords)

  (**
     [proc_file fname] is a list of words in [fname].
  *)
  let proc_file fname =
    let rec go acc in_channel =
      try
        let line = input_line in_channel in
        go (line :: acc) in_channel
      with
      | _ -> close_in in_channel; acc
    in
    go [] (open_in fname)
    |> List.map (fun line -> prewords line |> words)
    |> List.flatten

  (* [index_of_dir d] is an index of the files in [d].  Only files whose
   * names end in [.txt] are indexed.  Only [d] itself, not any
   * of its subdirectories, is indexed.
   * raises: Not_found if [d] is not a valid directory. *)
  let index_of_dir d =
    txt_in_dir d
    |> List.map (fun fn -> (proc_file fn, fn))
    |> List.fold_left (fun acc (ws, fn) ->
        List.fold_left (fun acc' w ->
            match D.find w acc' with
            | Some fns -> D.insert w (S.insert fn fns) acc'
            | None -> D.insert w (S.insert fn S.empty) acc')
          acc ws)
      D.empty

  (* [to_list idx] is a list representation of [idx] as an association
   * list.  The first element of each pair in the list is a word,
   * the second element is a list of the files in which that word
   * appears.  The order of elements in both the inner and outer
   * lists is unspecified.  Likewise, it is unspecified whether
   * the outer list contains multiple entries for words that
   * are the same other than case, or just a single entry. *)
  let to_list idx =
    D.to_list idx
    |> List.map (fun (w, fns) -> (w, S.to_list fns))

  (**
     [union ss] is the union of sets in [ss].
  *)
  let union = List.fold_left (fun acc s -> S.union acc s) S.empty

  (**
     [intersect ss] is the intersection of sets in [ss].
  *)
  let intersect = function
    | [] -> S.empty
    | s :: ss -> List.fold_left (fun acc s' -> S.intersect acc s') s ss

  (**
     [query scheme idx ws nots] is a list of file names containing words in [ws]
     except those in [nots].
     Containment of words in [ws] is defined by [scheme].
     Requires:
     - [ws] is not empty.
  *)
  let query scheme idx ws nots =
    (* List.is_empty added in 5.1... *)
    assert(List.length ws <> 0);
    let make_set w =
      let lc_w = String.lowercase_ascii w in
      match D.find lc_w idx with
      | Some fns -> fns
      | None -> S.empty
    in
    let yess = List.map make_set ws in
    let nos = List.map make_set nots in
    S.to_list (S.difference (scheme yess) (union nos))

  (* [or_not idx ors nots] is a list of the files that contain
   * any of the words in [ors] and none of the words in [nots].
   * requires: [ors] is not empty. *)
  let or_not = query union

  (* [and_not idx ands nots] is a list of the files that contain
   * all of the words in [ands] and none of the words in [nots].
   * requires: [ands] is not empty. *)
  let and_not = query intersect

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of an index
   * on the given formatter. *)
  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module TrivialEngine =
struct
  type idx = unit
  let index_of_dir d = ()
  let to_list idx = []
  let or_not idx ors nots = []
  let and_not idx ands nots = []
  let format fmt idx = ()
end

module StringComp
  : (Data.Comparable with type t = string)
= struct
  type t = string
  let compare s1 s2 =
    let comp = String.compare s1 s2 in
    if comp = 0 then
      `EQ
    else if comp < 0 then
      `LT
    else
      `GT
  let format _ _ = ()
end
module ListSet = Data.MakeSetOfDictionary(StringComp)(Data.MakeListDictionary)
module ListDict = Data.MakeListDictionary(StringComp)(ListSet)
module TreeSet = Data.MakeSetOfDictionary(StringComp)(Data.MakeTreeDictionary)
module TreeDict = Data.MakeTreeDictionary(StringComp)(TreeSet)

module ListEngine = MakeEngine(ListSet)(ListDict)
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)

module TreeEngine = MakeEngine(TreeSet)(TreeDict)
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)
