{
open Lexing
open FlareParser
open Regex

exception SyntaxError of string
}

let lit_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' ','] (* Adapting for non-unicode will probably just involve expanding this character class *)

rule read =
  parse
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '<'      { LEFT_BRACK }
  | '>'      { RIGHT_BRACK } (* TODO see if you can still use r, l, u, d inside regexes with this approach*)
  | 'r' '*'  { HORIZONTAL_CONSTRAINT    `RightAny }
  | 'r'+     { HORIZONTAL_CONSTRAINT    (`Right (String.length (Lexing.lexeme lexbuf))) }
  | 'l' '*'  { HORIZONTAL_CONSTRAINT    `LeftAny }
  | 'l'+     { HORIZONTAL_CONSTRAINT    (`Left  (String.length (Lexing.lexeme lexbuf))) }
  | 'u' '*'  { VERTICAL_CONSTRAINT      `UpAny }
  | 'u'+     { VERTICAL_CONSTRAINT      (`Up    (String.length (Lexing.lexeme lexbuf))) }
  | 'd' '*'  { VERTICAL_CONSTRAINT      `DownAny }
  | 'd'+     { VERTICAL_CONSTRAINT      (`Down  (String.length (Lexing.lexeme lexbuf))) }
  | '.'      { PERIOD }
  | lit_char { CHAR (Lexing.lexeme lexbuf).[0] }
  | '/'      { SLASH }
  | '*'      { STAR }
  | '|'      { OR_PIPE }
  | '?'      { QUESTION_MARK }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
