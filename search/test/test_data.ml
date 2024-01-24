open OUnit2
open Search
open Data
open QCheck

module type Tests = sig
  val tests : OUnit2.test list
end

(* Expose t for testing purposes *)
module Value : Formattable with type t = int = struct
  type t = int
  let format _ v = print_int v
end

module Key : Comparable with type t = int = struct
  type t = int
  let compare k1 k2 =
    if k1 = k2 then
      `EQ
    else if k1 < k2 then
      `LT
    else
      `GT
  let format _ k = print_int k
end

(*******************************************)
(*        Testing Utility Functions        *)
(*******************************************)

let string_of_int_opt = function
  | Some i -> "Some " ^ string_of_int i
  | None -> "None"

let string_of_kv (k, v) = "(" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"

let string_of_kv_list xs =
  let inside =
    List.fold_left (fun acc kv -> acc ^ ";" ^ string_of_kv kv) "" xs
  in
  "[" ^ inside ^ "]"

let string_of_int_list xs =
  let inside =
    List.fold_left (fun acc i -> acc ^ ";" ^ string_of_int i) "" xs
  in
  "[" ^ inside ^ "]"

let assert_kv_list_eq xs ys = fun _ ->
  assert_equal xs ys ~printer:string_of_kv_list

let assert_int_list_eq xs ys = fun _ ->
  assert_equal xs ys ~printer:string_of_int_list

let assert_int_opt_eq x y = fun _ ->
  assert_equal x y ~printer:string_of_int_opt

let assert_int_eq x y = fun _ ->
  assert_equal x y ~printer:string_of_int

let assert_true b = fun _ ->
  assert_bool "Expected true got false" b

let assert_false b = fun _ ->
  assert_bool " Expected false got true" (not b)

let assert_none o = fun _ ->
  assert_bool "Expected None got Some _" (Option.is_none o)

let f k v acc = (k,v) :: acc


(*******************************************)
(*                 Tests                   *)
(*******************************************)

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M:DictionaryMaker) = struct
  module Dict = M(Key)(Value)
  open Dict

  (* Generates a random canonical form of size *at most* n. *)
  let dict_gen =
    Gen.(sized @@ fix
           (fun self n ->
              match n with
              | 0 -> return empty
              | n -> map3 insert small_int small_int (self (n - 1))))
  let arbitrary_dict = make dict_gen

  let remove_empty_test =
    Test.make
      ~count:10
      ~name:"remove k empty test"
      (small_int)
      (fun k -> remove k empty = empty)
    |> QCheck_ounit.to_ounit2_test
  
  let member_empty_test =
    Test.make
      ~count:10
      ~name:"member k empty tests"
      (small_int)
      (fun k -> member k empty = false)
    |> QCheck_ounit.to_ounit2_test

  let find_empty_test =
    Test.make
      ~count:10
      ~name:"find k empty tests"
      (small_int)
      (fun k -> find k empty = None)
    |> QCheck_ounit.to_ounit2_test

  let insert_is_empty_test =
    Test.make
      ~count:10
      ~name:"is_empty (insert ...) test"
      (triple small_int small_int arbitrary_dict) (* k, v, d *)
      (fun (k, v, d) -> is_empty (insert k v d) = false)
    |> QCheck_ounit.to_ounit2_test

  let insert_size_test =
    Test.make
      ~count:10
      ~name:"size (insert ...) test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> size (insert k v d) >= size d)
    |> QCheck_ounit.to_ounit2_test

  let insert_shadow_size_test =
    Test.make
      ~count:10
      ~name:"insert shadow test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> size (insert k v (insert k 0 d)) = size (insert k 0 d))
    |> QCheck_ounit.to_ounit2_test

  let insert_member_test =
    Test.make
      ~count:10
      ~name:"member (insert ...) test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> member k (insert k v d) = true)
    |> QCheck_ounit.to_ounit2_test

  let insert_find_test =
    Test.make
      ~count:10
      ~name:"find k (insert ...) test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> find k (insert k v d) = Some v)
    |> QCheck_ounit.to_ounit2_test

  let insert_choose_test =
    Test.make
      ~count:10
      ~name:"choose (insert ...) test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> Option.is_some @@ choose (insert k v d))
    |> QCheck_ounit.to_ounit2_test

  let remove_unbind_test =
    Test.make
      ~count:10
      ~name:"remove_unbind_test"
      (triple small_int small_int arbitrary_dict)
      (fun (k, v, d) -> Option.is_none @@ find k (remove k d))
    |> QCheck_ounit.to_ounit2_test

  let member_can_be_found_test =
    Test.make
      ~count:10
      ~name:"member can be found test"
      (pair small_int arbitrary_dict)
      (fun (k, d) -> if member k d then
          Option.is_some @@ find k d
        else
          Option.is_none @@ find k d)
    |> QCheck_ounit.to_ounit2_test

  let can_be_found_member_test =
    Test.make
      ~count:10
      ~name:"member can be found test"
      (pair small_int arbitrary_dict)
      (fun (k, d) -> if Option.is_some @@ find k d then
          member k d
        else
          not (member k d))
    |> QCheck_ounit.to_ounit2_test

  let tests = [
    "empty is empty" >:: assert_true (is_empty empty);
    "empty has size 0" >:: assert_int_eq 0 (size empty);
    "choose empty is none" >:: assert_none (choose empty);
    "to_list empty is []" >:: (fun _ -> assert_equal [] (to_list empty));
    "fold empty is []" >:: (fun _ -> assert_equal [] (fold f [] empty));
    "insert shadows" >:: assert_int_opt_eq (Some 0)
      (find 1 (insert 1 0 (insert 1 1 empty)));
    "fold over a nonempty dict" >:: assert_kv_list_eq [(3,9);(2,8);(1,7)]
      (fold f [] (insert 3 9 (insert 2 8 (insert 1 7 empty))));
    "to_list a nonempty dict" >:: assert_kv_list_eq [(1,7);(2,8);(3,9)]
      (to_list (insert 3 9 (insert 2 8 (insert 1 7 empty))));
    remove_empty_test;
    member_empty_test;
    find_empty_test;
    insert_is_empty_test;
    insert_size_test;
    insert_shadow_size_test;
    insert_member_test;
    insert_find_test;
    insert_choose_test;
    remove_unbind_test;
    member_can_be_found_test;
    can_be_found_member_test
  ]
end


(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
module ListDictHarness = DictTester(MakeListDictionary)
module TreeDictHarness = DictTester(MakeTreeDictionary)
module TreeSet = MakeSetOfDictionary(Key)(MakeTreeDictionary)
open TreeSet
let tests = ListDictHarness.tests @ TreeDictHarness.tests @ [
    "empty is empty" >:: assert_true (is_empty empty);
    "not empty is not empty" >:: assert_false (is_empty (insert 0 empty));
    "empty has size 0" >:: assert_int_eq 0 (size empty);
    "{1,2,3} has size 3" >:: assert_int_eq 3
      (size (insert 1 (insert 2 (insert 3 empty))));
    "{1,2,2} has size 2" >:: assert_int_eq 2
      (size (insert 1 (insert 2 (insert 2 empty))));
    "1 is not a member of empty" >:: assert_false (member 1 empty);
    "1 is not a member of {2}" >:: assert_false (member 1 (insert 2 empty));
    "1 is a member of {1,2}" >:: assert_true
      (member 1 (insert 1 (insert 2 empty)));
    "removing 1 from empty is empty" >:: assert_true
      (is_empty (remove 1 empty));
    "removing 1 from {1} is empty" >:: assert_true
      (is_empty (remove 1 (insert 1 empty)));
    "removing 1 from {2} is {2}" >:: assert_int_list_eq [2]
      (to_list (remove 1 (insert 2 empty)));
    "union of disjoint sets" >:: assert_int_list_eq [1;2;3]
      (to_list (union (insert 1 empty) (insert 2 (insert 3 empty))));
    "union of nondisjoint sets" >:: assert_int_list_eq [1;2;3]
      (to_list (union (insert 1 (insert 3 empty)) (insert 2 (insert 3 empty))));
    "intersection of disjoint sets" >:: assert_true
      (is_empty (intersect (insert 1 empty) (insert 2 (insert 3 empty))));
    "intersection of nondisjoint sets" >:: assert_int_list_eq [3]
      (to_list
         (intersect (insert 1 (insert 3 empty)) (insert 2 (insert 3 empty))));
    "diff of disjoint sets" >:: assert_int_list_eq [1]
      (to_list (difference (insert 1 empty) (insert 2 (insert 3 empty))));
    "diff of nondisjoint sets" >:: assert_int_list_eq [1]
      (to_list
         (difference (insert 1 (insert 3 empty)) (insert 2 (insert 3 empty))));
  ]

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
