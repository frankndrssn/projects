open Gameengine

(**
 * First, prompt for a game to load.
 * Then, run a read-eval-print loop which:
 * 1. Asks for, then parses, a command from the user
 * 2. Processes that command using `Comand.parse` and `Command.run_command`
 * 3. Prints the resulting message to the user
 * until the user quits
*)

(**
   [front_matter game state] prints the front matter of a message.
   Requires:
   - [state] is in a running state.
*)
let front_matter game state =
  print_string "Turn: ";
  print_endline (string_of_int (State.num_turns game state));
  print_string "Items in this room: ";
  print_endline (String.concat ", " (State.items_in_cur_room game state))

(**
   [back_matter game state] prints the back matter of a message.
   Requires:
   - [state] is in a running state.
*)
let back_matter game state =
  if State.has_won game state then
    print_endline (State.win_msg game)
  else
    ()

(**
   [print_msg game msg state] prints front matter, [msg], and back matter.
*)
let print_msg game msg state =
  if State.is_running state then
    let () =
      front_matter game state;
      print_endline msg;
      back_matter game state
    in
    ()
  else
    print_endline msg

(**
   [repl game state] is the repl loop.
*)
let rec repl game state =
  if State.is_running state then
    let line = print_string "repl # "; read_line () in
    match Command.parse game line with
    | Ok cmd ->
      begin
        let (state', msg) = Command.run_command game state cmd in
        print_msg game msg state';
        repl game state'
      end
    | Error msg -> print_endline msg; repl game state
  else
    print_endline "No running game. Stopping."


let () = print_string "Game file: ";
  let fn = read_line () in
  (* Bootstrap the repl. *)
  match State.from_file fn with
  | Ok game ->
    begin
      let state = State.init_state game in
      let init_room = State.cur_room state in
      let desc =
        match State.desc_room init_room game state with
        | Ok desc -> desc
        | Error desc -> "Error :" ^ desc
      in
      print_endline "Game ok.";
      print_msg game desc state;
      repl game state
    end
  | Error msg -> print_endline msg


