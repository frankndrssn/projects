open Lexing

let print_position outx lexbuf = 
  let pos = lexbuf.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parser fmt lexbuf =
  try Some (parser Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
    Format.fprintf fmt "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Format.fprintf fmt "%a: syntax error\n" print_position lexbuf;
    None

let parse_expr_with_error = parse_with_error Parser.expr

let parse_typ_with_error = parse_with_error Parser.typ

let parse_command_with_error = parse_with_error Parser.com

let expr_of_string fmt s = 
  parse_expr_with_error fmt (Lexing.from_string s)

let expr_of_string_err = expr_of_string Format.err_formatter

let typ_of_string fmt s = 
  parse_expr_with_error fmt (Lexing.from_string s)

let typ_of_string_err = typ_of_string Format.err_formatter

let com_of_string fmt s = 
  parse_command_with_error fmt (Lexing.from_string s)

let com_of_string_err = com_of_string Format.err_formatter

let commands_of_file fmt fname =
  let ic = open_in fname in
  let rec go () = 
    begin try
      let line = input_line ic in
      begin match com_of_string fmt line with
      | Some c -> c :: go ()
      | None -> go ()
      end
    with
    | End_of_file -> close_in ic; []
    | e -> close_in_noerr ic; raise e
    end
  in go ()

let commands_of_file_err = commands_of_file Format.err_formatter