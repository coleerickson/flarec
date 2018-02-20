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

(* Precedences *)
%left STAR
%left CONCAT

%start <Json.value option> prog

%%
(* part 1 *)
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

regex:
  | LEFT_PAREN; r = regex; RIGHT_PAREN  { `Group r }
  | c = CHAR                            { `Char c }
  | { `Empty }
  | r = regex; STAR                     { `Repetition r }
  | r = regex; r2 = regex                 { `Concat (r, r2 ) } %prec CONCAT
  ;


  /* | LEFT_PAREN; r = regex; RIGHT_PAREN        { r } */
