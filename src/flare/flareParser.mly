%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_ANGLE_BRACK
%token RIGHT_ANGLE_BRACK
%token LEFT_SQUARE_BRACK
%token RIGHT_SQUARE_BRACK
%token <Regex.horizontal_constraint> HORIZONTAL_CONSTRAINT
%token <Regex.vertical_constraint> VERTICAL_CONSTRAINT
%token EOF
%token <char> CHAR
%token STAR
%token OR_PIPE
%token PERIOD
%token QUESTION_MARK
%token SLASH
%token COMMA
%token <token list> TOKEN_GROUP /* Not actually used, but necessary for FlareLexer*/

%start <Regex.flarex option> prog

%%
prog:
  /* | v = flarex; EOF { Some (`FConcat (`FCell ((`Repetition `Wildcard), false, `NoHorizontal, `NoVertical), v)) } */
  | v = flarex; EOF { Some v }
  | EOF       { None   }
  ;

flarex:
  /* | r = regex_wrapper { `FCell (r, true, `NoHorizontal, `NoVertical) } */
  | f1 = flarex; f2 = cell_or_alternation { `FConcat (f1, f2) }
  | f = cell_or_alternation                      { f }
  ;

horizontal_constraint:
    | h = HORIZONTAL_CONSTRAINT {h}
    | { `NoHorizontal }

vertical_constraint:
    | v = VERTICAL_CONSTRAINT {v}
    | { `NoVertical }

alternation:
    | f1 = alternation; COMMA; f2 = flarex { `FAlternation (f1, f2) }
    | f = flarex { f }

cell_or_alternation:
    | LEFT_SQUARE_BRACK; f = alternation; RIGHT_SQUARE_BRACK { f }
    | f = constrained_cell { f }

constrained_cell:
  | h = horizontal_constraint; v = vertical_constraint; LEFT_ANGLE_BRACK; r = regex_wrapper; RIGHT_ANGLE_BRACK  { `FCell (r, true, h, v) }  ;
  | h = horizontal_constraint; v = vertical_constraint; r = regex_wrapper { `FCell (r, false, h, v) }  ;

regex_wrapper:
  | SLASH; r = regex; SLASH   { r }
  ;


(* Inspired by BNF for regex at http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html *)
regex:
  | r1 = regex; OR_PIPE; r2 = simple     { `Alternation (r1, r2) }
  | r = simple                           { r }
  |                                      { `Empty }
  ;

simple:
  | r1 = simple; r2 = basic              { `Concat (r1, r2) }
  | r = basic                            { r }
  ;


(* TODO Check if we are able to use star and question mark in conjunction *)
basic:
  | r = elementary; STAR                 { `Repetition r }
  | r = elementary; QUESTION_MARK        { `Alternation (r, `Empty) }
  | r = elementary                       { r }
  ;

elementary:
  | LEFT_PAREN; r = regex; RIGHT_PAREN  { `Group r }
  | PERIOD                              { `Wildcard }
  | c = CHAR                            { `Char c }
  ;
