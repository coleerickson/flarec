%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF
%token <char> CHAR
%token SLASH
%token STAR
%token OR_PIPE
%token PERIOD
%token QUESTION_MARK

%start <Json.value option> prog

%%
prog:
  | v = value { Some v }
  | EOF       { None   } ;

value:
  | SLASH; r = regex; SLASH             { `Regex r    }
  | LEFT_BRACE; obj = obj_fields; RIGHT_BRACE { `Assoc obj  }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false }
  | NULL                                      { `Null       };

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = STRING; COLON; v = value              { (k, v) } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ;

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
