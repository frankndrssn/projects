(* Infix notation for bind. *)
let (>>=) = Result.bind

type command =
  | Go of string
  | Take of string
  | Drop of string
  | Quit
  | Look
  | Inventory
  | Score
  | Turns

(**
   [tokenize s] tokenizes [s].
   Letters are converted to lowercase and white spaces are trimmed.
*)
let tokenize s =
  String.(
    s
    |> lowercase_ascii
    |> split_on_char ' '
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  )

(**
   [parse game s] parses a command from [s].
   The parser finds the first command that it can parse.
   Returns:
   - [Ok cmd] if [s] can be parsed into a command.
   - [Error d] otherwise.
*)
let parse game s =
  let dirs = State.valid_dir game |> List.map String.lowercase_ascii in
  let dirs_str = String.concat ", " dirs in
  match tokenize s with
  | "go" :: [] -> Error "Command 'go' requires a direction"
  | "go" :: args -> Ok (Go (String.concat " " args))
  | "take" :: [] -> Error "Command 'take' requires an item"
  | "take" :: args -> Ok (Take (String.concat " " args))
  | "drop" :: [] -> Error "Command 'drop' requires an item"
  | "drop" :: args -> Ok (Drop (String.concat " " args))
  | "quit" :: _ -> Ok Quit
  | "look" :: _ -> Ok Look
  | "inventory" :: _ | "inv" :: _ -> Ok Inventory
  | "score" :: _ -> Ok Score
  | "turns" :: _ -> Ok Turns
  | args ->
    begin
      let d = (String.concat " " args) in
      if List.mem d dirs then
        Ok (Go d)
      else
        Error (d ^
               " cannot be interpreted as a command or a direction. \
                Possible directions: " ^ dirs_str)
    end

(**
   Extract a description from a [(string, string) result].
*)
let _desc = function
  | Ok desc -> desc
  | Error desc -> "Error: " ^ desc 

(**
   [desc_room] is the description of the current room.
   Requires:
   - [state] is in a running state.
*)
let desc_room game state =
  let cr = State.cur_room state in
  _desc @@ State.desc_room cr game state

(**
   [desc_item game id] is the description of [id].
*)
let desc_item game id = _desc @@ State.desc_item id game

(**
   [inventory game state] is a description of the current inventory.
   Requires:
   - [state] is in a running state.
*)
let inventory game state =
  let open List in
  State.inventory state
  |> map (fun id -> State.item_name id game)
  |> map _desc
  |> String.concat ", "


(**
   [run_command game state cmd] runs [cmd] in the given [state].
   Returns:
   - [(s, msg)] where [s] the the new state after running [cmd] and [msg] is
     a system message to be displayed.
*)
let run_command game state cmd =
  if State.is_running state then
    match cmd with
    | Go d ->
      begin
        match State.go d game state with
        | Ok state -> (state, desc_room game state)
        | Error msg -> (state, msg)
      end
    | Take item ->
      begin
        let result =
          State.find_item_by_name game item
          >>= (fun iid ->
              Result.map
                (fun r -> (r, iid))
                (State.take iid game state))
        in
        match result with
        | Ok (state, iid) ->
          (state, "You picked up an item - " ^ desc_item game iid)
        | Error msg -> (state, msg)
      end
    | Drop item ->
      begin
        let result =
          State.find_item_by_name game item
          >>= (fun iid -> State.drop iid game state)
        in
        match result with
        | Ok state -> (state, "You dropped an item")
        | Error msg -> (state, msg)
      end
    | Quit -> (State.quit game state, "")
    | Look -> (state, desc_room game state)
    | Inventory -> (state, "Inventory: " ^ (inventory game state))
    | Score -> (state, "Your score: " ^ string_of_int (State.cur_score state))
    | Turns -> (state, "Turns: " ^ string_of_int (State.num_turns game state))
  else
    (State.stop, "There is no active game.")
