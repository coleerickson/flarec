open Core
open Lexer
open FlareLexer
open Lexing
open Regex

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_regex_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_and_print lexbuf =
  match parse_regex_with_error lexbuf with
  | Some value ->
    printf "%a\n" Regex.output_value value
  | None -> ()

let parse_file_and_print filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let parse_regex_exn s =
  match (parse_regex_with_error (Lexing.from_string s)) with
  | Some (`Regex r) -> r
  | _ -> raise (Invalid_argument "Failed to parse regex")

let parse_regex_to_nfa_exn s = regex_to_nfa (parse_regex_exn s)

let match_regex regex_s s =
    eval s (parse_regex_to_nfa_exn regex_s)

let parse_regex_to_dfa_exn s = nfa_to_dfa (parse_regex_to_nfa_exn s)

let parse_flarex_with_error lexbuf =
  try FlareParser.prog FlareLexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_flarex_exn s =
  match (parse_flarex_with_error (Lexing.from_string s)) with
  | Some f -> f
  | _ -> raise (Invalid_argument "Failed to parse regex")

let print_match_regex regex s =
  Printf.printf "regex: %s\ns: %s\n" regex s;
  Printf.printf "%s\n" (if match_regex regex s then "match" else "no match");
  ()
