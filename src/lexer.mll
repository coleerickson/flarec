{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
(* let int = '-'? ['0'-'9'] ['0'-'9']* *)

(* part 2 *)
(* let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp? *)

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let c = ['a'-'z' 'A'-'Z' '0'-'9']

(* part 4 *)
rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | '.'       { QUESTION_MARK }
  | '*'       { STAR }
  | '.'       { PERIOD }
  | '|'       { OR_PIPE }
  | c         { CHAR (Lexing.lexeme lexbuf).[0] }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { EOF }
