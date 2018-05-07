{
open Lexing
open FlareParser
open Regex

exception SyntaxError of string
}

let lit_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' ','] (* Adapting for non-unicode will probably just involve expanding this character class *)

rule raw_read =
  parse
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '<'      { LEFT_BRACK }
  | '>'      { RIGHT_BRACK }
  | 'r' '*'  { HORIZONTAL_CONSTRAINT    `RightAny }
  | 'r'+     { HORIZONTAL_CONSTRAINT    (`Right (String.length (Lexing.lexeme lexbuf))) }
  | 'l' '*'  { HORIZONTAL_CONSTRAINT    `LeftAny }
  | 'l'+     { HORIZONTAL_CONSTRAINT    (`Left  (String.length (Lexing.lexeme lexbuf))) }
  | 'u' '*'  { VERTICAL_CONSTRAINT      `UpAny }
  | 'u'+     { VERTICAL_CONSTRAINT      (`Up    (String.length (Lexing.lexeme lexbuf))) }
  | 'd' '*'  { VERTICAL_CONSTRAINT      `DownAny }
  | 'd'+     { VERTICAL_CONSTRAINT      (`Down  (String.length (Lexing.lexeme lexbuf))) }
  | '.'      { PERIOD }
  | '/'     { TOKEN_GROUP (read_regex [SLASH] lexbuf) }
  | '*'      { STAR }
  | '|'      { OR_PIPE }
  | '?'      { QUESTION_MARK }
  | _ { raise (SyntaxError ("Unexpected char while lexing Flare program: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_regex regex_tokens =
  parse
  | '\\' '/' { read_regex (regex_tokens @ [CHAR '/']) lexbuf }
  | '/'      { regex_tokens @ [SLASH] }
  | '('      { read_regex (regex_tokens @ [LEFT_PAREN]) lexbuf }
  | ')'      { read_regex (regex_tokens @ [RIGHT_PAREN]) lexbuf }
  | '.'      { read_regex (regex_tokens @ [PERIOD]) lexbuf }
  | lit_char { read_regex (regex_tokens @ [CHAR (Lexing.lexeme lexbuf).[0] ]) lexbuf }
  | '*'      { read_regex (regex_tokens @ [STAR]) lexbuf }
  | '|'      { read_regex (regex_tokens @ [OR_PIPE]) lexbuf }
  | '?'      { read_regex (regex_tokens @ [QUESTION_MARK]) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char while lexing regex: " ^ Lexing.lexeme lexbuf)) }

{

(* https://stackoverflow.com/a/3438015 *)
let read =
  let l = ref [] in
  fun lexbuf ->
    match !l with
    | x::xs -> l := xs; x
    | [] -> match raw_read lexbuf with
            | TOKEN_GROUP (x::xs) -> l := xs; x
            | token -> token
}
