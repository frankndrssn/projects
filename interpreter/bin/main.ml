open Ruse.Ast
open Ruse.Parse
open Ruse.Interp

let vnum = "v. 1.0.0"

let impossible_msg = "The \"impossible\" happened: typechecking succeeded, "^
                     "but evaluation failed"

let typecheck_failure_msg = "Type checking failed"

let process_command fmt env gamma quitstring = function
| Some (Typeof e) ->
  begin match typeof gamma e with
  | Some t -> pretty_typ' fmt t; 
    Some (env, gamma)
  | None -> 
    Format.fprintf fmt "%s" typecheck_failure_msg; 
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  end
| Some (Eval e) ->
  begin match typeof gamma e, interpret env e with
  | Some _, Some v -> 
    pretty_value' fmt v;
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  | Some _, None -> 
    Format.fprintf fmt "%s" impossible_msg;
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  | None, _ -> 
    Format.fprintf fmt "%s" typecheck_failure_msg;
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  end
| Some (Def (x, e)) ->
  begin match typeof gamma e, interpret env e with
  | Some t, Some v ->
    Format.fprintf fmt "%s = %a : %a" x pretty_value' v pretty_typ' t;
    Format.pp_print_newline fmt ();
    Some (add_binding x v env, add_type_binding x t gamma)
  | Some _, None -> 
    Format.fprintf fmt "%s" impossible_msg;
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  | None, __->
    Format.fprintf fmt "%s" typecheck_failure_msg;
    Format.pp_print_newline fmt ();
    Some (env, gamma)
  end
| Some QuitRepl ->
  Format.fprintf fmt "%s" quitstring;
  Format.pp_print_newline fmt ();
  None
| None ->
  Format.fprintf fmt "%s" "Bad command";
  Format.pp_print_newline fmt ();
  Some (env, gamma)

let rec repl fmt env gamma =
  Format.fprintf fmt "%s" "> ";
  Format.pp_print_flush fmt ();
  let ln = read_line () in
  let c = parse_command_with_error fmt (Lexing.from_string ln) in
  match process_command Format.std_formatter env gamma "Goodbye!" c with
  | Some (env', gamma') -> repl fmt env' gamma'
  | None -> ()


let () = 
  print_endline ("Welcome to Ruse (" ^ vnum ^  ")");
  print_endline ("Commands:");
  print_endline ("  `:eval e` or just `e`: evaluate e and print the value to the screen");
  print_endline ("  `:typeof e`: print the type of e to the screen");
  print_endline ("  `:def x e` evaluate e, and bind the result to x in for the rest of the session");
  print_endline ("  `:quit` or `:q`: quit Ruse, ending the session");
  repl Format.std_formatter empty_env empty_tenv
