module Intset = Set.Make(Int)

type conditioned_desc = {
  itemids : int list;
  desc : string
}

(* exit is a keyword *)
type eksit = {
  direction : string;
  room : int;
  to_unlock : int list
}

type room = {
  id : int;
  descs : conditioned_desc list;
  points : int;
  exits : eksit list;
  to_drop : int list
}

type item = {
  name : string;
  id : int;
  desc : string;
  points : int
}

type item_loc = {
  item_id : int;
  room_id : int
}

type game = {
  rooms : room list;
  items : item list;
  start : int;
  starting_inventory : int list;
  item_locs : item_loc list;
  winning_message : string
}

type snapshot = {
  cur_room : room;
  cur_inventory : int list;
  cur_item_locs : item_loc list;
  visited : Intset.t;
  turns : int;
  cur_score : int
}

type state =
  | GNext of snapshot
  | GStop

(* Infix notations for bind *)
let (>>-) = Option.bind
let (>>=) = Result.bind

(* Upgrades an option to a result with [err] *)
let (>>+) opt err =
  match opt with
  | Some d -> Ok d
  | None -> Error err

(************************************************)
(*             Utility functions                *)
(************************************************)

module type Utility = sig
  val winning_score : game -> int
  val find_item_by_id : game -> int -> item option
  val find_room_by_id : game -> int -> room option
  val find_item_id_by_name : game -> string -> int option
  val recompute_score : game -> snapshot -> snapshot
  val sci_eq : string -> string -> bool
  val items_in_room : snapshot -> int -> int list
  val remove_first : 'a -> 'a list -> 'a list
end

module GameUtility : Utility = struct
  (**
     [item_score_map game] is a list of item locations that earn the player
     points.
  *)
  let item_score_map game =
    game.rooms
    |> List.map (fun (room : room) -> (room.id, room.to_drop))
    |> List.map
      (fun (room_id, to_drop) ->
         List.map (fun iid -> { room_id; item_id = iid }) to_drop)
    |> List.flatten

  (**
     [sci_eq s1 s2] is [s1 = s2] but ignores case.
  *)
  let sci_eq s1 s2 =
    let s1' = String.lowercase_ascii s1 in
    let s2' = String.lowercase_ascii s2 in
    s1' = s2'

  (**
     [winning_score game] is the winning score of [game].
  *)
  let winning_score game =
    List.map (fun (room : room) -> room.points) game.rooms
    @ List.map (fun item -> item.points) game.items
    |> List.fold_left (+) 0

  (**
     [find_item_by_id game id] finds an item in [game] whose id is [id].
  *)
  let find_item_by_id game id =
    List.find_opt (fun item -> item.id = id) game.items

  (**
     [find_room_by_id game id] finds a room in [game] whose id is [id].
  *)
  let find_room_by_id game id =
    List.find_opt (fun (room : room) -> room.id = id) game.rooms

  (**
     [find_item_id_by_name game name] finds an item's id whose name is [name].
  *)
  let find_item_id_by_name game name =
    List.find_opt (fun item -> sci_eq item.name name) game.items
    >>- (fun item -> Some (item.id))

  (**
     [recompute_score game snapshot] recomputes the current score given
     [snapshot].
     Requires:
     - [game] and [snapshot] are valid.
  *)
  let recompute_score game snapshot =
    let room_score =
      Intset.elements snapshot.visited
      |> List.map (find_room_by_id game)
      |> List.map Option.get
      |> List.map (fun (room : room) -> room.points)
      |> List.fold_left (+) 0
    in
    let ism = item_score_map game in
    let item_score =
      List.filter (fun loc -> List.mem loc ism) snapshot.cur_item_locs
      |> List.map (fun loc -> loc.item_id)
      |> List.map (find_item_by_id game)
      |> List.map Option.get
      |> List.map (fun item -> item.points)
      |> List.fold_left (+) 0
    in
    { snapshot with cur_score = room_score + item_score }

  (**
     [items_in_room snapshot id] is a list of item ids in room with id [id].
  *)
  let items_in_room snapshot id =
    List.filter (fun loc -> loc.room_id = id) snapshot.cur_item_locs
    |> List.map (fun loc -> loc.item_id)

  (**
     [remove_first x ls] removes the first occurrence of [x] in [ls].
     Requires:
     - [x] is in [ls].
  *)
  let rec remove_first x = function
    | [] -> failwith "list does not contain the arg"
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (remove_first x ys)
end

(************************************************)
(*                Game checker                  *)
(************************************************)

module type Checker = sig
  val check_game : game -> (game, string) result
  val game_ok : game -> game
  val snapshot_ok : game -> snapshot -> snapshot
end

module GameChecker : Checker = struct
  let debug = true
    
  (**
     [room_ids game] produces a list of all room ids in [game].
  *)
  let room_ids game =
    game.rooms
    |> List.map (fun (room : room) -> room.id)

  (**
     [item_ids game] produces a list of all item ids in [game].
  *)
  let item_ids game =
    game.items
    |> List.map (fun (item : item) -> item.id)

  (**
     [item_ids game] produces a list of all item names in [game].
  *)
  let item_names game =
    game.items
    |> List.map (fun item -> item.name)

  (**
     [room_ids_descs rooms] is the set of item ids that appear in the
     descriptions of some room in [rooms].
  *)
  let item_ids_descs rooms =
    rooms
    |> List.map (fun room -> room.descs)
    |> List.flatten
    |> List.map (fun desc -> desc.itemids)
    |> List.flatten
    |> Intset.of_list

  (**
     [room_ids_exits rooms] is the set of room ids of some exit in [game].
  *)
  let room_ids_exits rooms =
    rooms
    |> List.map (fun room -> room.exits)
    |> List.flatten
    |> List.map (fun eksit -> eksit.room)
    |> Intset.of_list

  (**
     [room_ids_locs locs] is the set of room ids where some items are located
     in [locs].
  *)
  let room_ids_locs locs =
    locs
    |> List.map (fun loc -> loc.room_id)
    |> Intset.of_list

  (**
     [item_ids_locs locs] is the set of item ids located in some room in [locs].
  *)
  let item_ids_locs locs =
    locs
    |> List.map (fun loc -> loc.item_id)
    |> Intset.of_list

  (**
     [is_uniq compare ls] checks whether [ls] contains duplicates according to
     [compare].
     Returns true if it does not.
  *)
  let is_uniq compare ls =
    let ls' = List.sort compare ls in
    let ls'u = List.sort_uniq compare ls in
    ls' = ls'u

  (**
     [_check_game checker emsg game] checks whether [game] is valid according
     to [checker]. Returns and error with message [emsg] if not valid.
  *)
  let _check_game checker emsg game =
    if checker game then
      Ok game
    else
      Error emsg

  (**
     Checks whether the exits of [room] have unique directions.
  *)
  let _check_exits_dir room =
    List.map (fun eksit -> eksit.direction) room.exits
    |> is_uniq String.compare

  (**
     [game_ok] is [Ok game] if ...
  *)
  let check_game game =
    let rids = room_ids game in
    let srids = Intset.of_list rids in
    let iids = item_ids game in
    let siids = Intset.of_list iids in
    _check_game
      (fun _ -> is_uniq Int.compare rids)
      "Bad game file: room ids not unique"
      game >>=
    _check_game
      (fun _ -> is_uniq Int.compare iids)
      "Bad game file: item ids not unique" >>=
    _check_game
      (fun game -> is_uniq String.compare (item_names game))
      "Bad game file: item names not unique" >>=
    _check_game
      (fun game -> List.mem game.start rids)
      "Bad game file: starting room does not exist" >>=
    _check_game
      (fun game -> Intset.subset (item_ids_descs game.rooms) siids)
      "Bad game file: room description contains a nonexistent item" >>=
    _check_game
      (fun game -> Intset.subset (room_ids_exits game.rooms) srids)
      "Bad game file: one of the exits is a nonexistent room" >>=
    _check_game
      (fun game -> Intset.subset (Intset.of_list game.starting_inventory) siids)
      "Bad game file: the inventory contains a nonexistent item" >>=
    _check_game
      (fun game -> Intset.subset (room_ids_locs game.item_locs) srids)
      "Bad game file: some items are located in a nonexistent room" >>=
    _check_game
      (fun game -> Intset.subset (item_ids_locs game.item_locs) siids)
      "Bad game file: some nonexistent items are located in some room" >>=
    _check_game
      (fun game -> List.for_all _check_exits_dir game.rooms)
      "Bad game file: some rooms have exits with the same direction"

  (**
     [game_ok game] is [game] if [game] is valid.
  *)
  let game_ok game =
    if debug then
      match check_game game with
      | Ok game -> game 
      | Error _ -> failwith "invalid game"
    else
      game

  (**
     [snapshot_ok game snapshot] is [snapshot] if [snapshot] is valid.
  *)
  let snapshot_ok game snapshot =
    if debug then
      let rids = room_ids game in
      let srids = Intset.of_list rids in
      let iids = item_ids game in
      let siids = Intset.of_list iids in
      assert (List.mem snapshot.cur_room.id rids);
      assert (Intset.subset (Intset.of_list snapshot.cur_inventory) siids);
      assert (Intset.subset (room_ids_locs snapshot.cur_item_locs) srids);
      assert (Intset.subset (item_ids_locs snapshot.cur_item_locs) siids);
      assert (Intset.subset snapshot.visited srids);
      snapshot
    else
      snapshot
end

(************************************************)
(*                Parser                        *)
(************************************************)

module type Parser = sig
  val parse_game : Yojson.Basic.t -> (game, string) result
end

module GameParser : Parser = struct
  (**
     [extract_list prop json] extract a list named [prop] from [json].
  *)
  let extract_list (prop : string) (json : Yojson.Basic.t)
    : Yojson.Basic.t list =
    Yojson.Basic.Util.(json |> member prop |> to_list)

  (**
     [extract_int prop json] extracts a int named [prop] from [json].
  *)
  let extract_int (prop : string) (json : Yojson.Basic.t) : int =
    Yojson.Basic.Util.(json |> member prop |> to_int)

  (**
     [extract_string prop json] extracts a string named [prop] from [json].
  *)
  let extract_string (prop : string) (json : Yojson.Basic.t) : string =
    Yojson.Basic.Util.(json |> member prop |> to_string)

  (**
     [parse_exits json] parses an exit entry in [json].
  *)
  let parse_exit (json : Yojson.Basic.t) = {
    direction = extract_string "direction" json;
    room = extract_int "room" json;
    to_unlock =
      List.map Yojson.Basic.Util.to_int (extract_list "to_unlock" json)
  }

  (**
     [parse_conditioned_desc json] parses a description entry in [json].
  *)
  let parse_conditioned_desc (json : Yojson.Basic.t) = {
    itemids = List.map Yojson.Basic.Util.to_int (extract_list "itemids" json);
    desc = extract_string "desc" json
  }

  (**
     [parse_room json] parses a room entry in [json].
  *)
  let parse_room (json : Yojson.Basic.t) = {
    id = extract_int "id" json;
    descs = List.map parse_conditioned_desc @@ extract_list "descs" json;
    points = extract_int "points" json;
    exits = List.map parse_exit @@ extract_list "exits" json;
    to_drop = List.map Yojson.Basic.Util.to_int @@ extract_list "to_drop" json
  }

  (**
     [parse_item json] parses an item entry in [json].
  *)
  let parse_item (json : Yojson.Basic.t) : item = {
    name = extract_string "name" json;
    id = extract_int "id" json;
    desc = extract_string "desc" json;
    points = extract_int "points" json
  }

  (**
     [parse_item_loc json] parses an item location entry in [json].
  *)
  let parse_item_loc (json : Yojson.Basic.t) : item_loc = {
    item_id = extract_int "item_id" json;
    room_id = extract_int "room_id" json
  }

  (**
     [parse_game json] parses [json] into a game.
  *)
  let parse_game (json : Yojson.Basic.t) : (game, string) result =
    try
      let rooms = List.map parse_room @@ extract_list "rooms" json in
      let items = List.map parse_item @@ extract_list "items" json in
      Ok {
        rooms = rooms;
        items = items;
        start = extract_int "start" json;
        starting_inventory =
          List.map Yojson.Basic.Util.to_int @@
          extract_list "starting_inventory" json;
        item_locs = List.map parse_item_loc @@ extract_list "item_locs" json;
        winning_message = extract_string "winning_message" json
      }
    with
    | _ -> Error "Failed to parse the given game file"
end

(************************************************)
(*             Required functions               *)
(************************************************)

(**
   [from_file fn] produces a game from a json file [fn].
   Returns an [Error] if the file can't be parsed.
*)
let from_file fn : (game, string) result =
  try
    let json = Yojson.Basic.from_file fn in
    GameParser.parse_game json >>=
    GameChecker.check_game
  with
  | _ -> Error ("Failed to load: " ^ fn)

(**
   [from_file fn] produces a game from a string [str].
   Returns an [Error] if the file can't be parsed.
*)
let from_string str : (game, string) result =
  try
    let json = Yojson.Basic.from_string str in
    GameParser.parse_game json >>=
    GameChecker.check_game
  with
  | _ -> Error ("Failed to understand: " ^ str)

(**
   [win_msg game] is the winning message of [game].
*)
let win_msg game = game.winning_message

(**
   [init_state game] is the initial state of [game].
   Initially
   - the player is in the starting room
   - the score is 0
   - the player has taken 0 turns
     Requires:
   - [game] is valid.
*)
let init_state game =
  let _ = GameChecker.game_ok game in
  let cur_room = game.start |> GameUtility.find_room_by_id game |> Option.get in
  let snapshot = {
    cur_room;
    cur_inventory = game.starting_inventory;
    cur_item_locs = game.item_locs;
    visited = Intset.singleton game.start;
    turns = 0;
    cur_score = 0
  }
  in
  GNext (snapshot
         |> GameUtility.recompute_score game
         |> GameChecker.snapshot_ok game)

(**
   [desc_item id game] finds an item's description whose id is [id] in [game].
   Returns:
   - [Ok desc] if the item is found.
   - [Error s] if the item is not found.
*)
let desc_item id game =
  let _ = GameChecker.game_ok game in
  match GameUtility.find_item_by_id game id with
  | Some item -> Ok (item.desc)
  | None -> Error ("Could not find item: " ^ string_of_int id)

(**
   [item_name id game] finds an item name whose id is [id] in [game].
   Returns:
   - [Ok name] if the item is found.
   - [Error s] if the item is not found.
*)
let item_name id game =
  let _ = GameChecker.game_ok game in
  match GameUtility.find_item_by_id game id with
  | Some item -> Ok (item.name)
  | None -> Error ("Could not find item: " ^ string_of_int id)

(**
   [find_item_by_name game item_name] finds an item based on the name that a
   player has given you.
   Returns:
   - [Ok n] if item is found
   - [Error s] is item is not found
     Requires:
   - [game] has only one item for every name. 
*)
let find_item_by_name game name =
  let _ = GameChecker.game_ok game in
  match GameUtility.find_item_id_by_name game name with
  | Some id -> Ok id
  | None -> Error ("Could not find: " ^ name)

(**
   [desc_room room_id game] describes room [room_id] based on the player's
   current inventory.
   Returns:
   - [Ok d] if [d] is the description of  the room with id [room_id] when the
     player has inventory [inventory s].
   - [Error s] if no such room is found.
     Requires:
   - [state] is [GNext snapshot].
*)
let desc_room id game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      GameUtility.find_room_by_id game id >>+
      ("Could not find room: " ^ string_of_int id)
      >>= (fun room ->
          let items = (* the set of items in the room [id] & inventory *)
            (GameUtility.items_in_room snapshot id
             @ snapshot.cur_inventory)
            |> Intset.of_list
          in
          List.find_opt (fun cdesc ->
              Intset.(subset (of_list cdesc.itemids) items))
            room.descs
          >>- (fun cdesc -> Some cdesc.desc)
          >>+ "Bad game file: Nonexhaustive precondition.")
    end
  | GStop -> failwith "Game has already ended."

(**
   [exits room_id game] gets the list of exits for a room.
   Returns:
   - [Some exits] if room [room_id] can be found and [exits] are the room ids
     of its exits.
   - [None] if not such room can be found.
*)
let exits id game =
  let _ = GameChecker.game_ok game in
  GameUtility.find_room_by_id game id
  >>- (fun room ->
      Some (room.exits |> List.map (fun eksit -> eksit.room)))

(**
   [cur_score state] is the current score.
   Requires:
   - [state] is a running state.
*)
let cur_score = function
  | GNext snapshot -> snapshot.cur_score
  | GStop -> failwith "Game has already ended."

(**
   [cur_room state] gets the room id of the room the player is currently in.
   Requires:
   - [state] is a running state.
*)
let cur_room = function
  | GNext snapshot -> snapshot.cur_room.id
  | GStop -> failwith "Game has already ended."

(**
   [inventory state] gets the list of item ids for all of the ids in the
   players inventory.
   Guarantees:
   - item ids are given in decreasing order.
   Requires:
   - [state] is a running state
*)
let inventory = function
  | GNext snapshot ->
    List.sort (fun a b -> Int.(-compare a b)) snapshot.cur_inventory
  | GStop -> failwith "Game has already ended."

(**
   [go d game state] moves in a direction indicated by [d].
   Returns:
   - [Ok s] if [s] is the result of the player moving in the [d] direction from
     [cur_room s].
   - [Error s] if [d] is not valid or the player has not yet unlocked the exit.
     Requires:
   - [state] is [GNext snapshot].
*)
let go d game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      let dirs = List.map
          (fun eksit -> eksit.direction)
          snapshot.cur_room.exits
      in
      List.find_opt
        (fun eksit -> GameUtility.sci_eq d eksit.direction)
        snapshot.cur_room.exits
      >>+ d ^
          " is not a valid direction. Valid directions: " ^
          String.concat ", " dirs
      >>= (fun eksit ->
          let to_unlock = Intset.of_list eksit.to_unlock in
          let items = Intset.of_list (snapshot.cur_inventory @
                                      GameUtility.items_in_room
                                        snapshot
                                        snapshot.cur_room.id)
          in
          if Intset.subset to_unlock items then
            Ok (eksit)
          else
            Error "Find more items to unlock this room.")
      >>= (fun eksit ->
          let dest = GameUtility.find_room_by_id game eksit.room
                     |> Option.get
          in
          let new_snap = {
            snapshot with
            cur_room = dest;
            visited = Intset.add dest.id snapshot.visited;
            turns = snapshot.turns + 1
          } in
          Ok (GNext (new_snap
                     |> GameUtility.recompute_score game
                     |> GameChecker.snapshot_ok game)))
    end
  | GStop -> failwith "Game has already ended."

(**
   [take id game state] takes the item indicated by [item_id] into the
   inventory.
   Returns:
   - [Ok s] if the item can be found in the current room.
     In the new state, the found item is removed from the current room and
     moved into the inventory.
   - [Error s] if
     + the item is not in the current room
     + the item does not exist
     Requires:
   - [state] is a running state.
*) 
let take id game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      let iids = GameUtility.items_in_room snapshot snapshot.cur_room.id in
      List.find_opt (fun iid -> iid = id) iids
      >>+ "This item is not in this room. Try something else."
      >>= (fun iid ->
          let new_snap = {
            snapshot with
            cur_inventory = iid :: snapshot.cur_inventory;
            cur_item_locs =
              GameUtility.remove_first { item_id = iid;
                                         room_id = snapshot.cur_room.id }
                snapshot.cur_item_locs;
            turns = snapshot.turns + 1
          } in
          Ok (GNext (new_snap
                     |> GameUtility.recompute_score game
                     |> GameChecker.snapshot_ok game)))
    end
  | GStop -> failwith "Game has already ended."

(**
   [drop id game state] drops an item [id] from the inventory.
   Returns:
   - [Ok s] where
     + one occurrence of [item_id] is removed from the player's inventory.
     + [item_id] is now in the current room.
   - [Error s] if [item_id] is not in the players inventory or it does not
     exist.
     Requires:
   - [state] is a running state.
*)
let drop id game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      List.find_opt (fun iid -> iid = id) snapshot.cur_inventory
      >>+ "You do not own this item."
      >>= (fun iid ->
          let new_snap = {
            snapshot with
            cur_inventory = GameUtility.remove_first iid snapshot.cur_inventory;
            cur_item_locs =
              { item_id = iid; room_id = snapshot.cur_room.id } ::
              snapshot.cur_item_locs;
            turns = snapshot.turns + 1
          } in
          Ok (GNext (new_snap
                     |> GameUtility.recompute_score game
                     |> GameChecker.snapshot_ok game)))
    end
  | GStop -> failwith "Game has already ended."

(**
   [has_won game state] determines whether the player has won in the current
   state.
   Requires:
   - [state] is a running state.
*)
let has_won game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      snapshot.cur_score >= GameUtility.winning_score game
    end
  | GStop -> failwith "Game has already ended."

(**
   [num_turns] is the number of turns the player has played.
   Requires:
   - [state] is a running state.
*)
let num_turns game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      snapshot.turns
    end
  | GStop -> failwith "Game has already ended."

(**
   [quit game state] quits the game.
   Requires:
   - [state] is a running state.
*)
let quit game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      GStop
    end
  | GStop -> failwith "Game has already ended."

(**
   [is_running state] is true if [state] is a running state.
*)
let is_running = function
  | GNext _ -> true
  | GStop -> false

(**
   [valid_dir game] is a list of valid directions in [game].
*)
let valid_dir game =
  let _ = GameChecker.game_ok game in
  let open List in
  map (fun room -> room.exits) game.rooms
  |> flatten
  |> map (fun eksit -> eksit.direction)
  |> sort_uniq String.compare

(**
   [items_in_cur_room game state] is a list of item names in the current room.
   Requires:
   - [state] is a running state
*)
let items_in_cur_room game = function
  | GNext snapshot ->
    begin
      let _ = GameChecker.game_ok game in
      let _ = GameChecker.snapshot_ok game snapshot in
      let open List in
      GameUtility.items_in_room snapshot snapshot.cur_room.id
      |> map (GameUtility.find_item_by_id game)
      |> map Option.get
      |> map (fun item -> item.name)
    end
  | GStop -> failwith "Game has already ended."

(**
   [stop] is a terminated state.
*)
let stop = GStop
