%token <char> CHAR
%token LEFT_PAREN
%token RIGHT_PAREN
%token QUESTION_MARK
%token STAR
%token PERIOD
%token OR_PIPE
%token EOF

%start <Regex.regex> prog

%%
(* part 1 *)
prog:
  | r = regex { r }
  | EOF       { `Empty  } ;

regex:
  | r1 = regex; r2 = regex { `Concat (r1, r2) }
  | g = group; STAR { `Repetition g }
  /* | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | TRUE                                      { `Bool true  }
  */
  | PERIOD                                      { `Wildcard       }
  | c = CHAR                                     { `Char c } ;

group:
  | LEFT_PAREN; r = regex; RIGHT_PAREN { r }
  | LEFT_PAREN; RIGHT_PAREN { `Empty  }
/*
alternation:
  | LEFT_PAREN; r = regex; RIGHT_PAREN { `Assoc obj  }
  | LEFT_PAREN; RIGHT_PAREN { `Empty  }


obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = STRING; COLON; v = value              { (k, v) } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ; */
