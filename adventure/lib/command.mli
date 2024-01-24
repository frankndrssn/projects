type command

(**
 * Parse a command from a string.
 * 
 * `parse g s` results in a command if `s` is a properly-formed command input.
 * Otherwise, it returns a descriptive error message. 
 *)
val parse : State.game -> string -> (command, string) result

(**
 * Run a command in a state.
 * 
 * `run_command g s c` returns `(s', msg)` where:
 * `s'` is the new state after running command `c` in state `s`
 * `msg` is the message to be displayed to the user
 *)
val run_command : State.game -> State.state -> command -> State.state * string
