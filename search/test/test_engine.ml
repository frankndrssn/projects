open OUnit2
open Search
open Engine

open TreeEngine

let sep = Filename.dir_sep
let root = Sys.getcwd () ^ sep (* .../_built/default/test/ *)
           ^ ".." ^ sep (* .../_built/default/ *)
           ^ ".." ^ sep (* .../_built/ *)
           ^ ".." ^ sep (* .../ *)
           ^ "examplefiles" ^ sep
           ^ "test1" ^ sep
let idx = index_of_dir root

let string_list_printer xs =
  let inside =
    List.fold_left (fun acc x -> acc ^ ";" ^ x) "" xs
  in
  "[" ^ inside ^ "]"

let assert_string_list_eq xs ys = fun _ ->
  let xs_s = List.sort (String.compare) xs in
  let ys_s = List.sort (String.compare) ys in
  assert_equal xs_s ys_s ~printer:string_list_printer

let tests = [
  "or(we, the, people, united)" >:: assert_string_list_eq
    [root ^ "small.txt"; root ^ "medium.txt"]
    (or_not idx ["we"; "the"; "people"; "united"] []);
  "or(we, the, people, united) not(america)" >:: assert_string_list_eq
    [root ^ "small.txt"]
    (or_not idx ["we"; "the"; "people"] ["america"]);
  "and(we, the, people, united)" >:: assert_string_list_eq
    [root ^ "medium.txt"]
    (and_not idx ["we"; "the"; "people"; "united"] []);
  "and(we, the, people, united) not(america)" >:: assert_string_list_eq
    []
    (and_not idx ["we"; "the"; "people"; "united"] ["america"]);
]

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
