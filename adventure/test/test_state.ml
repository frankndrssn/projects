open OUnit2
open Gameengine

(********************************************)
(*                Game Setup                *)
(********************************************)

(** The root of the project. The ..s get us out of the test and build directories.
    This allows us to get to the example_games directory *)
let root = Sys.getcwd () ^ "/../example_games/"

let file_from_root fn = root ^ fn

let one_room_game = 
  match State.from_file (file_from_root "one_room.json") with
  | Ok g -> g
  | Error s -> failwith ("Error Parsing the One-Room Game: " ^ s)

let org_init = State.init_state one_room_game

let two_room_game = 
  match State.from_file (file_from_root "two_rooms.json") with
  | Ok g -> g
  | Error s -> failwith ("Error Parsing the Two-Room Game: " ^ s)

let trg_init = State.init_state two_room_game


let small_circle_game =
  match State.from_file (file_from_root "small_circle.json") with
  | Ok g -> g
  | Error s -> failwith ("Error Parsing the Small-Circle Game: " ^ s)

let scg_init = State.init_state small_circle_game

let tcs_game =
  match State.from_file (file_from_root "tcs.json") with
  | Ok g -> g
  | Error s -> failwith ("Error Parsing tcs game " ^ s)

let tcs_init = State.init_state tcs_game

(*******************************************)
(*        Testing Utility Functions        *)
(*******************************************)

let pass = assert_equal 0 0

let string_res_printer = function
| Ok s -> "Ok: " ^ s
| Error s -> "Error: " ^ s

let int_res_printer = function
  | Ok i -> "Ok: " ^ (string_of_int i)
  | Error s -> "Error: " ^ s

let int_list_printer_int x xs = 
  List.fold_left (fun s y -> s ^ "; " ^ (string_of_int y)) (string_of_int x) xs

let int_list_printer = function
| [] -> "[]"
| (x :: xs) -> "[" ^ int_list_printer_int x xs ^ "]"

let option_printer (p : 'a -> string) = function
| Some x -> "Some: " ^ p x
| None -> "None"

let assert_string_eq s1 s2 =
  (fun _ -> assert_equal s1 s2 ~printer:(fun x -> x))

let assert_bool_eq b1 b2 =
  (fun _ -> assert_equal b1 b2 ~printer:string_of_bool)

let assert_int_eq x y =
  (fun _ -> assert_equal x y ~printer:string_of_int)

let assert_string_res_eq x y =
  (fun _ -> assert_equal x y ~printer:string_res_printer)

let assert_int_res_eq x y =
  (fun _ -> assert_equal x y ~printer:int_res_printer)

let assert_int_list_eq x y =
  (fun _ -> assert_equal x y ~printer:int_list_printer)

let assert_option_int_list_eq x y =
  (fun _ -> assert_equal x y ~printer:(option_printer int_list_printer))

let assert_ok x =
  (fun _ -> match x with | Ok _ -> assert_bool "" true | Error e -> assert_failure ("Expected Ok, got (Error " ^ e ^")"))

let assert_error x = (fun _ ->
    match x with
    | Ok _ -> assert_failure ("Expected Error, got Ok _")
    | Error _ -> pass
  )

let play_with_errors g prev_st cmd_maybe =
  match prev_st with
  | Error e -> Error ("Previous state error: " ^ e)
  | Ok (st, msg) ->
    begin match cmd_maybe with
    | Error e -> Error ("Command parse error: " ^ e)
    | Ok cmd ->
      let (st', msg') = Command.run_command g st cmd in
      Ok (st', msg ^ "\n" ^ msg')
    end

let next_turn turn tt =
  match turn with
  | Error _ -> []
  | Ok (st, _) -> tt st

(***********************************************)
(*               command parse test            *)
(***********************************************)

let parse_tcs_cmd = Command.parse tcs_game

let command_parse_tests =
  "command parse tests" >:::
  [
    "parse go north"
    >:: (assert_ok (parse_tcs_cmd "go north"));
    "parse go north west"
    >:: (assert_ok (parse_tcs_cmd "go SoUth"));
    "parse north"
    >:: (assert_ok (parse_tcs_cmd "NeXt RooM"));
    "parse go"
    >:: (assert_error (parse_tcs_cmd "go"));
    "parse take item"
    >:: (assert_ok (parse_tcs_cmd "take beer and bacon"));
    "parse take"
    >:: (assert_error (parse_tcs_cmd "take"));
    "parse drop item"
    >:: (assert_ok (parse_tcs_cmd "drop beer and bacon"));
    "parse inventory"
    >:: (assert_ok (parse_tcs_cmd "inventory"));
    "parse inv"
    >:: (assert_ok (parse_tcs_cmd "inv"));
    "parse look"
    >:: (assert_ok (parse_tcs_cmd "look"));
    "parse score"
    >:: (assert_ok (parse_tcs_cmd "score"));
    "parse quit"
    >:: (assert_ok (parse_tcs_cmd "quit"));
    "parse empty string"
    >:: (assert_error (parse_tcs_cmd ""));
    "parse upper case"
    >:: (assert_ok (parse_tcs_cmd "GO NORTH"));
    "parse mix case"
    >:: (assert_ok (parse_tcs_cmd "TakE Beer aNd Bacon"))
  ]

(***********************************************)
(*               misc tests                    *)
(***********************************************)

let desc_item_tests =
  "desc item tests" >:::
  [
    "item 0" >:: assert_string_res_eq
      (Ok "A small circular gem. It looks like it would fit into the altar in the first room.")
      (State.desc_item 0 small_circle_game);
    "item 1" >:: assert_string_res_eq
      (Ok "A small square gem. It looks like it would fit into the altar in the first room.")
      (State.desc_item 1 small_circle_game)
  ] @
  List.init 1000 (
    fun _ ->
      let id = Random.int 10000 - Random.int 5000 in
      if id = 0 || id = 1 then
        "nothing to test" >:: fun _ -> pass
      else
        string_of_int id ^ " does not exist" >:: assert_error
          (State.desc_item id small_circle_game)
  )

let item_name_tests =
  "item name tests" >:::
  [
    "item 0" >:: assert_string_res_eq
      (Ok "Circular Gem")
      (State.item_name 0 small_circle_game);
    "item 1" >:: assert_string_res_eq
      (Ok "Square Gem")
      (State.item_name 1 small_circle_game)
  ] @
  List.init 1000 (
    fun _ ->
      let id = Random.int 10000 - Random.int 5000 in
      if id = 0 || id = 1 then
        "nothing to test" >:: fun _ -> pass
      else
        string_of_int id ^ " does not exist" >:: assert_error
          (State.item_name id small_circle_game)
  )

let bad_names = List.init 1000 (
    fun i ->
      String.init i (
        fun _ ->
          Random.int 256 |> Char.chr
      )
  ) |> List.filter (
    fun name ->
      let name' = String.lowercase_ascii name in
      name' <> "circular gem" || name' <> "square gem"
  )

let find_item_by_name_tests =
  "find item by name tests" >:::
  [
    "Circular Gem" >:: assert_int_res_eq
      (Ok 0)
      (State.find_item_by_name small_circle_game "Circular Gem");
    "cIrCuLaR gEm" >:: assert_int_res_eq
      (Ok 0)
      (State.find_item_by_name small_circle_game "cIrCuLaR gEm");
    "Square Gem" >:: assert_int_res_eq
      (Ok 1)
      (State.find_item_by_name small_circle_game "Square Gem");
    "square gem" >:: assert_int_res_eq
      (Ok 1)
      (State.find_item_by_name small_circle_game "square gem")
  ] @ List.init 1000 (
    fun _ ->
      "random capitalization of circular gem"
      >:: assert_int_res_eq
        (Ok 0)
        (State.find_item_by_name small_circle_game @@
           (String.map (fun c -> if Random.bool () then
                           Char.lowercase_ascii c
                         else
                           Char.uppercase_ascii c)) "circular gem")
  )
  @ List.map (fun name ->
      "bad name: " ^ name >:: assert_error
        (State.find_item_by_name small_circle_game name)
    ) bad_names

let exits_tests =
  "exits tests" >:::
  [
    "exits of altar"
    >:: assert_option_int_list_eq (Some [1;2])
      (State.exits 0 small_circle_game);
    "exits of room 1"
    >:: assert_option_int_list_eq (Some [0;2])
      (State.exits 1 small_circle_game);
    "exits of room 2"
    >:: assert_option_int_list_eq (Some [0;1])
      (State.exits 2 small_circle_game);
  ] @
  List.init 1000 (
    fun _ ->
      let id = Random.int 10000 - Random.int 5000 in
      if id = 0 || id = 1 || id = 2 then
        "nothing to test" >:: fun _ -> pass
      else
        string_of_int id ^ " does not exist" >:: assert_option_int_list_eq
          (None) (State.exits id small_circle_game)
  )

(***********************************************)
(*               One-room tests                *)
(***********************************************)

let one_room_parse = "One Room Parsing Tests" >:::
[
  "Winning message" >:: (assert_string_eq 
                          ("You won!") 
                          (State.win_msg one_room_game));
  "Starting Room" >:: (assert_int_eq 0 (State.cur_room org_init));
  "Describe Room" >:: (assert_string_res_eq 
                        (Ok "The only room in this adventure.")
                        (State.desc_room 0 one_room_game org_init));
  "Exits" >:: (assert_option_int_list_eq 
                (Some []) (State.exits 0 one_room_game));
  "Nonexistant Exits" >:: (assert_option_int_list_eq
                            None (State.exits 1 one_room_game));
  "You've already won" >:: (assert_bool_eq true (State.has_won one_room_game org_init))
]

(***********************************************)
(*               Two-room tests                *)
(***********************************************)

(* Play the game *)

let go_other_cmd = Command.parse two_room_game "go other"
let go_first_cmd = Command.parse two_room_game "go first"

let turn1 =
  match go_other_cmd with
  | Ok cmd -> Ok (Command.run_command two_room_game trg_init cmd)
  | Error e -> Error ("Command parse error: " ^ e)

let turn2 = play_with_errors two_room_game turn1 go_first_cmd

let turn3 = play_with_errors two_room_game turn2 go_other_cmd

let two_room_play = "Two-Room Play Tests" >:::
[
  "Parse \"go other\"" >:: (assert_ok go_other_cmd);
  "Parse \"go first\"" >:: (assert_ok go_other_cmd);
  "Play turn 1" >:: (assert_ok turn1);
  "Play turn 2" >:: (assert_ok turn2);
  "Play turn 3" >:: (assert_ok turn3)
] @
match turn1 with
| Error _ -> [] (* We already have detected this if it's a problem! *)
| Ok (st1, _) ->
  [
    "In other room" >:: (assert_int_eq 1 (State.cur_room st1));
    "We won" >:: (assert_bool_eq true (State.has_won two_room_game st1));
    "We have one point" >:: (assert_int_eq 1 (State.cur_score st1));
    "Empty inventory" >:: (assert_int_list_eq [] (State.inventory st1));
  ] @
  match turn2 with
  | Error _ -> [] (* Already tested *)
  | Ok (st2, _) -> (* Importantly, we didn't take away the loss *)
    [
      "In first room" >:: (assert_int_eq 0 (State.cur_room st2));
      "We won" >:: (assert_bool_eq true (State.has_won two_room_game st2));
      "We have one point" >:: (assert_int_eq 1 (State.cur_score st2));
      "Empty inventory" >:: (assert_int_list_eq [] (State.inventory st2))
    ] @
    match turn3 with
    | Error _ -> []
    | Ok (st3, _) -> (* Importantly, we haven't added more points! *)
      [
        "In other room" >:: (assert_int_eq 1 (State.cur_room st3));
        "We won" >:: (assert_bool_eq true (State.has_won two_room_game st3));
        "We have one point" >:: (assert_int_eq 1 (State.cur_score st3));
        "Empty inventory" >:: (assert_int_list_eq [] (State.inventory st3));
      ]

(***********************************************)
(*               small-circle tests            *)
(***********************************************)

let play_with_errors_sc = play_with_errors small_circle_game
let onward = Command.parse small_circle_game "onward"
let back = Command.parse small_circle_game "back"
let altar = Command.parse small_circle_game "go altar"
let go_nonexist = Command.parse small_circle_game "go does not exist"
let take_circular = Command.parse small_circle_game "take circular gem"
let take_square = Command.parse small_circle_game "take square gem"
let take_nonexist = Command.parse small_circle_game "take does not exist"
let drop_circular = Command.parse small_circle_game "drop circular gem"
let drop_square = Command.parse small_circle_game "drop square gem"
let drop_nonexist = Command.parse small_circle_game "drop does not exist"

let turn1 =
  match onward with
  | Ok cmd -> Ok (Command.run_command small_circle_game scg_init cmd)
  | Error e -> Error e

let turn2 = play_with_errors_sc
    turn1
    take_circular

let turn2' = play_with_errors_sc
    turn2
    take_circular

let turn2'' = play_with_errors_sc
    turn2'
    take_square

let turn3 = play_with_errors_sc
    turn2''
    onward

let turn3' = play_with_errors_sc
    turn3
    go_nonexist

let turn4 = play_with_errors_sc
    turn3'
    take_square

let turn5 = play_with_errors_sc
    turn4
    altar

let turn6 = play_with_errors_sc
    turn5
    drop_circular

let turn7 = play_with_errors_sc
    turn6
    drop_square

let turn7' = play_with_errors_sc
    turn7
    drop_square

let turn7'' = play_with_errors_sc
    turn7'
    drop_nonexist

let turn7''' = play_with_errors_sc
    turn7''
    take_nonexist

let turn8 = play_with_errors_sc
    turn7''
    take_square

let turn9 = play_with_errors_sc
    turn8
    back

let turn10 = play_with_errors_sc
    turn9
    back

let turn11 = play_with_errors_sc
    turn10
    back

let sc_play =
  "small circle play" >:::
  [
    "parse 'onward'" >:: assert_ok onward;
    "parse 'back'" >:: assert_ok back;
    "parse 'altar'" >:: assert_ok altar;
    "parse 'go does not exist'" >:: assert_ok go_nonexist;
    "parse 'take circular gem'" >:: assert_ok take_circular;
    "parse 'take square gem'" >:: assert_ok take_square;
    "parse 'take does not exist'" >:: assert_ok take_nonexist;
    "parse 'drop circular gem'" >:: assert_ok drop_circular;
    "parse 'drop square gem'" >:: assert_ok drop_square;
    "parse 'drop does not exist'" >:: assert_ok drop_nonexist
  ]
  @ [
    "T0: desc current room"
    >:: (assert_string_res_eq (Ok "You find yourself in a small \
                                   room. There is an altar in the \
                                   middle with two slots, one \
                                   square and one circle. A closed \
                                   door reads \"exit\". Two open \
                                   doors lead to two rooms, each \
                                   seemingly empty except for a \
                                   shine in the middle of the \
                                   floor.")
           (State.desc_room 0 small_circle_game scg_init));
    "desc nonexist room"
    >:: (assert_error (State.desc_room 3 small_circle_game scg_init));
    "T0: score"
    >:: (assert_int_eq 0 (State.cur_score scg_init));
    "T0: current room"
    >:: (assert_int_eq 0 (State.cur_room scg_init));
    "T0: inventory"
    >:: (assert_int_list_eq [] (State.inventory scg_init));
    "T0: has_won"
    >:: (assert_bool_eq false (State.has_won small_circle_game scg_init));
    "T0: turns"
    >:: (assert_int_eq 0 (State.num_turns small_circle_game scg_init))
  ]
  @ next_turn turn1 (fun st -> [
        "T1: desc current room"
        >:: (assert_string_res_eq (Ok "This is where you found the circular \
                                       gem. There are two doors: one to the \
                                       room with an altar, and another to a \
                                       room with a square gem sitting on the \
                                       floor.")
               (State.desc_room 1 small_circle_game st));
        "T1: score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T1: current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T1: inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T1: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T1: turns"
        >:: (assert_int_eq 1 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn2 (fun st -> [
        "T2: score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T2: current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T2: inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T2: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T2: turns"
        >:: (assert_int_eq 2 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn2' (fun st -> [
        "T2': score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T2': current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T2': inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T2': has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T2': turns"
        >:: (assert_int_eq 2 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn2'' (fun st -> [
        "T2'': score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T2'': current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T2'': inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T2'': has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T2'': turns"
        >:: (assert_int_eq 2 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn3 (fun st -> [
        "T3: desc current room"
        >:: (assert_string_res_eq (Ok "This is where you found the square gem. \
                                       There are two doors: one to the room \
                                       with an altar, and another to the room \
                                       where you found the circular gem.")
               (State.desc_room 2 small_circle_game st));
        "T3: score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T3: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T3: inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T3: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T3: turns"
        >:: (assert_int_eq 3 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn3' (fun st -> [
        "T3': desc current room"
        >:: (assert_string_res_eq (Ok "This is where you found the square gem. \
                                       There are two doors: one to the room \
                                       with an altar, and another to the room \
                                       where you found the circular gem.")
               (State.desc_room 2 small_circle_game st));
        "T3': score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T3': current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T3': inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T3': has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T3': turns"
        >:: (assert_int_eq 3 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn4 (fun st -> [
        "T4: score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T4: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T4: inventory"
        >:: (assert_int_list_eq [1;0] (State.inventory st));
        "T4: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T4: turns"
        >:: (assert_int_eq 4 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn5 (fun st -> [
        "T5: desc current room"
        >:: (assert_string_res_eq (Ok "You're back in the room you started in. \
                                       The gems in your hands start to glow \
                                       and vibrate. It's as if they want to be \
                                       placed in the slots on the altar in \
                                       front of you.")
               (State.desc_room 0 small_circle_game st));
        "T5: score"
        >:: (assert_int_eq 0 (State.cur_score st));
        "T5: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T5: inventory"
        >:: (assert_int_list_eq [1;0] (State.inventory st));
        "T5: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T5: turns"
        >:: (assert_int_eq 5 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn6 (fun st -> [
        "T6: score"
        >:: (assert_int_eq 50 (State.cur_score st));
        "T6: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T6: inventory"
        >:: (assert_int_list_eq [1] (State.inventory st));
        "T6: has won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T6: turns"
        >:: (assert_int_eq 6 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn7 (fun st -> [
        "T7: score"
        >:: (assert_int_eq 100 (State.cur_score st));
        "T7: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T7: inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T7: has_won"
        >:: (assert_bool_eq true (State.has_won small_circle_game st));
        "T7: turns"
        >:: (assert_int_eq 7 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn7' (fun st -> [
        "T7': score"
        >:: (assert_int_eq 100 (State.cur_score st));
        "T7': current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T7': inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T7': has_won"
        >:: (assert_bool_eq true (State.has_won small_circle_game st));
        "T7': turns"
        >:: (assert_int_eq 7 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn7'' (fun st -> [
        "T7'': score"
        >:: (assert_int_eq 100 (State.cur_score st));
        "T7'': current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T7'': inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T7'': has_won"
        >:: (assert_bool_eq true (State.has_won small_circle_game st));
        "T7'': turns"
        >:: (assert_int_eq 7 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn7''' (fun st -> [
        "T7''': score"
        >:: (assert_int_eq 100 (State.cur_score st));
        "T7''': current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T7''': inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T7''': has_won"
        >:: (assert_bool_eq true (State.has_won small_circle_game st));
        "T7''': turns"
        >:: (assert_int_eq 7 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn8 (fun st -> [
        "T8: score"
        >:: (assert_int_eq 50 (State.cur_score st));
        "T8: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T8: inventory"
        >:: (assert_int_list_eq [1] (State.inventory st));
        "T8: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T8: turns"
        >:: (assert_int_eq 8 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn9 (fun st -> [
        "T9: desc current room"
        >:: (assert_string_res_eq (Ok "This is where you found the square gem. \
                                       There are two doors: one to the room \
                                       with an altar, and another to a room \
                                       with a circular gem sitting on the \
                                       floor.")
               (State.desc_room 2 small_circle_game st));
        "T9: score"
        >:: (assert_int_eq 50 (State.cur_score st));
        "T9: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T9: inventory"
        >:: (assert_int_list_eq [1] (State.inventory st));
        "T9: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T9: turns"
        >:: (assert_int_eq 9 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn10 (fun st -> [
        "T10: desc current room" (* if the spec says so... *)
        >:: (assert_string_res_eq (Ok "In front of you lays a small circular \
                                       gem. There are two doors: one to the \
                                       room with an altar, and another to a \
                                       room with a square gem sitting on the \
                                       floor.")
               (State.desc_room 1 small_circle_game st));
        "T10: score"
        >:: (assert_int_eq 50 (State.cur_score st));
        "T10: current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T10: inventory"
        >:: (assert_int_list_eq [1] (State.inventory st));
        "T10: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T10: turns"
        >:: (assert_int_eq 10 (State.num_turns small_circle_game st))
      ])
  @ next_turn turn11 (fun st -> [
        "T11: desc current room" (* if the spec says so... *)
        >:: (assert_string_res_eq (Ok "You're back in the room you started in. \
                                       The gems in your hands start to glow \
                                       and vibrate. It's as if they want to be \
                                       placed in the slots on the altar in \
                                       front of you.")
               (State.desc_room 0 small_circle_game st));
        "T11: score"
        >:: (assert_int_eq 50 (State.cur_score st));
        "T11: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T11: inventory"
        >:: (assert_int_list_eq [1] (State.inventory st));
        "T11: has_won"
        >:: (assert_bool_eq false (State.has_won small_circle_game st));
        "T11: turns"
        >:: (assert_int_eq 11 (State.num_turns small_circle_game st))
      ])

(***********************************************)
(*               tcs tests                     *)
(***********************************************)

let play_with_errors_tcs = play_with_errors tcs_game
let left = Command.parse tcs_game "left"
let right = Command.parse tcs_game "right"
let next_room = Command.parse tcs_game "next room"
let take_b = Command.parse tcs_game "take item b"
let take_c = Command.parse tcs_game "take item c"
let drop_a = Command.parse tcs_game "drop item a"
let drop_b = Command.parse tcs_game "drop item b"
let drop_c = Command.parse tcs_game "drop item c"

let turn0' =
  match left with
  | Ok cmd -> Ok (Command.run_command tcs_game tcs_init cmd)
  | Error e -> Error e

let turn1 = play_with_errors_tcs
    turn0'
    right

let turn2 = play_with_errors_tcs
    turn1
    next_room

let turn3 = play_with_errors_tcs
    turn2
    take_b

let turn4 = play_with_errors_tcs
    turn3
    next_room

let turn5 = play_with_errors_tcs
    turn4
    left

let turn6 = play_with_errors_tcs
    turn5
    take_c

let turn7 = play_with_errors_tcs
    turn6
    next_room

let turn8 = play_with_errors_tcs
    turn7
    next_room

let turn9 = play_with_errors_tcs
    turn8
    drop_a

let turn10 = play_with_errors_tcs
    turn9
    drop_b

let turn11 = play_with_errors_tcs
    turn10
    drop_c

let turn12 = play_with_errors_tcs
    turn11
    left

let tcs_play =
  "tcs play" >:::
  [
    "parse 'left'" >:: assert_ok left;
    "parse 'right'" >:: assert_ok right;
    "parse 'next room'" >:: assert_ok next_room;
    "parse 'take item b'" >:: assert_ok take_b;
    "parse 'take item c'" >:: assert_ok take_c;
    "parse 'drop item a'" >:: assert_ok drop_a;
    "parse 'drop item b'" >:: assert_ok drop_b;
    "parse 'drop item c'" >:: assert_ok drop_c
  ]
  @ [
    "T0: score"
    >:: (assert_int_eq 1 (State.cur_score tcs_init));
    "T0: current room"
    >:: (assert_int_eq 0 (State.cur_room tcs_init));
    "T0: inventory"
    >:: (assert_int_list_eq [0] (State.inventory tcs_init));
    "T0: has_won"
    >:: (assert_bool_eq false (State.has_won tcs_game tcs_init));
    "T0: turns"
    >:: (assert_int_eq 0 (State.num_turns tcs_game tcs_init))
  ]
  @ next_turn turn0' (fun st -> [
        "T0': score"
        >:: (assert_int_eq 1 (State.cur_score st));
        "T0': current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T0': inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T0': has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T0': turns"
        >:: (assert_int_eq 0 (State.num_turns tcs_game st))
      ])
  @ next_turn turn1 (fun st -> [
        "T1: score"
        >:: (assert_int_eq 2 (State.cur_score st));
        "T1: current room"
        >:: (assert_int_eq 4 (State.cur_room st));
        "T1: inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T1: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T1: turns"
        >:: (assert_int_eq 1 (State.num_turns tcs_game st))
      ])
  @ next_turn turn2 (fun st -> [
        "T2: score"
        >:: (assert_int_eq 3 (State.cur_score st));
        "T2: current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T2: inventory"
        >:: (assert_int_list_eq [0] (State.inventory st));
        "T2: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T2: turns"
        >:: (assert_int_eq 2 (State.num_turns tcs_game st))
      ])
  @ next_turn turn3 (fun st -> [
        "T3: score"
        >:: (assert_int_eq 3 (State.cur_score st));
        "T3: current room"
        >:: (assert_int_eq 1 (State.cur_room st));
        "T3: inventory"
        >:: (assert_int_list_eq [1;0] (State.inventory st));
        "T3: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T3: turns"
        >:: (assert_int_eq 3 (State.num_turns tcs_game st))
      ])
  @ next_turn turn4 (fun st -> [
        "T4: score"
        >:: (assert_int_eq 3 (State.cur_score st));
        "T4: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T4: inventory"
        >:: (assert_int_list_eq [1;0] (State.inventory st));
        "T4: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T4: turns"
        >:: (assert_int_eq 4 (State.num_turns tcs_game st))
      ])
  @ next_turn turn5 (fun st -> [
        "T5: score"
        >:: (assert_int_eq 4 (State.cur_score st));
        "T5: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T5: inventory"
        >:: (assert_int_list_eq [1;0] (State.inventory st));
        "T5: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T5: turns"
        >:: (assert_int_eq 5 (State.num_turns tcs_game st))
      ])
  @ next_turn turn6 (fun st -> [
        "T6: score"
        >:: (assert_int_eq 4 (State.cur_score st));
        "T6: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T6: inventory"
        >:: (assert_int_list_eq [2;1;0] (State.inventory st));
        "T6: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T6: turns"
        >:: (assert_int_eq 6 (State.num_turns tcs_game st))
      ])
  @ next_turn turn7 (fun st -> [
        "T7: score"
        >:: (assert_int_eq 5 (State.cur_score st));
        "T7: current room"
        >:: (assert_int_eq 3 (State.cur_room st));
        "T7: inventory"
        >:: (assert_int_list_eq [2;1;0] (State.inventory st));
        "T7: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T7: turns"
        >:: (assert_int_eq 7 (State.num_turns tcs_game st))
      ])
  @ next_turn turn8 (fun st -> [
        "T8: score"
        >:: (assert_int_eq 5 (State.cur_score st));
        "T8: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T8: inventory"
        >:: (assert_int_list_eq [2;1;0] (State.inventory st));
        "T8: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T8: turns"
        >:: (assert_int_eq 8 (State.num_turns tcs_game st))
      ])
  @ next_turn turn9 (fun st -> [
        "T9: score"
        >:: (assert_int_eq 6 (State.cur_score st));
        "T9: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T9: inventory"
        >:: (assert_int_list_eq [2;1] (State.inventory st));
        "T9: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T9: turns"
        >:: (assert_int_eq 9 (State.num_turns tcs_game st))
      ])
  @ next_turn turn10 (fun st -> [
        "T10: score"
        >:: (assert_int_eq 7 (State.cur_score st));
        "T10: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T10: inventory"
        >:: (assert_int_list_eq [2] (State.inventory st));
        "T10: has_won"
        >:: (assert_bool_eq false (State.has_won tcs_game st));
        "T10: turns"
        >:: (assert_int_eq 10 (State.num_turns tcs_game st))
      ])
  @ next_turn turn11 (fun st -> [
        "T11: score"
        >:: (assert_int_eq 8 (State.cur_score st));
        "T11: current room"
        >:: (assert_int_eq 0 (State.cur_room st));
        "T11: inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T11: has_won"
        >:: (assert_bool_eq true (State.has_won tcs_game st));
        "T11: turns"
        >:: (assert_int_eq 11 (State.num_turns tcs_game st))
      ])
  @ next_turn turn12 (fun st -> [
        "T11: desc current room" (* if the spec says so... *)
        >:: (assert_string_res_eq (Ok "ha you're stuck")
               (State.desc_room 2 tcs_game st));
        "T12: score"
        >:: (assert_int_eq 8 (State.cur_score st));
        "T12: current room"
        >:: (assert_int_eq 2 (State.cur_room st));
        "T12: inventory"
        >:: (assert_int_list_eq [] (State.inventory st));
        "T12: has_won"
        >:: (assert_bool_eq true (State.has_won tcs_game st));
        "T12: turns"
        >:: (assert_int_eq 12 (State.num_turns tcs_game st))
      ])

(*******************************************************************************************)
(*                                      Run the Tests                                      *)
(*******************************************************************************************)

let _ =
  run_test_tt_main command_parse_tests;
  run_test_tt_main one_room_parse;
  run_test_tt_main desc_item_tests;
  run_test_tt_main item_name_tests;
  run_test_tt_main find_item_by_name_tests;
  run_test_tt_main exits_tests;
  run_test_tt_main two_room_play;
  run_test_tt_main sc_play;
  run_test_tt_main tcs_play
