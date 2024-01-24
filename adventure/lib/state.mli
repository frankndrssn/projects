(* The type of persistant information about the game. *)
type game

(* The type of ephemeral game state. *)
type state 

(**
 *  JSON Parsing.
 *
 * `from_file fname` parses the JSON game found in the file `fname`.
 * Returns `Ok g` when `g` is the result of parsing the JSON found in `fname`.
 * Returns `Error s` when there is an error parsing the game, where `s` is a
 * string describing the error.
*)
val from_file : string -> (game, string) result

(**
 * JSON Parsing.
 *
 * `from_string s` parses the JSON game found in the string `s`.
 * Other than the source of the JSON, it behaves as `from_file`, above.
*)
val from_string : string -> (game, string) result

(**
 * Get the winning message of a game. Printed when the game is won.
*)
val win_msg : game -> string

(**
 * Get the initial state of a game. 
 *
 * The initial state should have the player in the current room,
 * with a score of 0 and having taken 0 turns. 
*)
val init_state : game -> state

(**
 * Describe an item in the game.
 *
 * `desc_item n g` returns `Ok desc` if item `n` in game `g` has description `desc`.
 * If item `n` does not exist in game `g`, then `desc_item n g` returns an `Error` 
 * with a descriptive message.
*)
val desc_item : int -> game -> (string, string) result

(**
 * Get an item's name.
 *
 * `item_name g i` returns `Ok s` if the item with id `i` has name `s`.
 * If there is no such item, it returns a descriptive message.
*)
val item_name : int -> game -> (string, string) result

(**
 * Find an item based on the name that a player has given you.
 *
 * `find_item_by_name g s` returns `Ok n` if the (unique) item named `s` in `g` has
 * id `n`.
 * If there is no such item, it returns a descriptive message. 
 * 
 * Prerequesite: `game` has only one item for every name. 
*)
val find_item_by_name : game -> string -> (int, string) result

(**
 * Describe a room based on the player's current inventory.
 *
 * `desc_room i g s` returns `Ok d` if `d` is the description of 
 * the room with id `i` when the player has inventory `inventory s`.
 * If there is no such room, it returns a descriptive message.   
*)
val desc_room : int -> game -> state -> (string, string) result 

(**
 * Get the list of exits for a room.
 *
 * `exits i g` returns `Some l` where `l` is the list of room
 * ids for all exits from the room with id `i`.
 * If there is no room with id `i`, it returns `None`.
*)
val exits : int -> game -> int list option

(**
 * Gets the current score.
*)
val cur_score : state -> int

(**
 * Gets the room id of the room the player is currently in.
*)
val cur_room : state -> int

(**
 * Gets the list of item ids for all of the ids in the players inventory.
 *
 * Guarantees that item ids are given in decreasing order.
*)
val inventory : state -> int list 

(**
 * Move in a direction indicated by the first argument.
 *
 * `go d g s` returns `Ok s'` if `s'` is the result of
 * the player moving in the `d` direction from `cur_room s`.
 * 
 * If the direction `d` is not valid, it returns a descriptive message.   
*)
val go : string -> game -> state -> (state, string) result

(**
 * Take the item indicated by the first argument into inventory.
 * 
 * `take i g s` returns `Ok s'`, where:
 * in `s'`, the item with id `i` is no longer in any room,
 * in `s'`, the item id `i` is now in the player's inventory
 *
 * If the item `i` is not in the room `cur_room s`, or it
 * does not exist, then `take i g s` returns a descriptive message.
*)
val take : int -> game -> state -> (state, string) result

(**
 * Drop the item indicated by the first argument in the current room.
 * 
 * `drop i g s` returns `Ok s'` where:
 * in `s'`, the item with id `i` is no longer in the player's inventory
 * in `s'`, the item with id `i` is in `cur_room s`
 * 
 * If the item `i` is not in the players inventory, or it
 * does not exist, then `drop i g s` returns a descriptive message.
*)
val drop : int -> game -> state -> (state, string) result 

(**
 * Returns whether the player has won in the current state. 
*)
val has_won : game -> state -> bool

(**
 * Returns the number of turns the player has currently played
*)
val num_turns : game -> state -> int

(**
 * Quits the game.
 *
 * `quit s` returns `s'` where `s'` represents a quit state.Stdlib
*)
val quit : game -> state -> state

(*******************************************)
(*          additional functions           *)
(*******************************************)

(**
   [is_running state] is true if [state] is in a running state.
*)
val is_running : state -> bool

(**
   [valid_dir game] is a list of valid directions in [game].
*)
val valid_dir : game -> string list

(**
   [items_in_cur_room game state] is a list of item names in the current room.
*)
val items_in_cur_room : game -> state -> string list

(**
   [stop] is a terminated state.
*)
val stop : state
