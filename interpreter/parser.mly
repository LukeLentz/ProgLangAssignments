%{
  open Types
%}

%token IF THEN ELSE
%token OR AND NOT
%token TRUE FALSE
%token <float> FLOAT
%token DBLSEMI
%nonassoc FLOAT
%nonassoc ELSE
%left OR AND
%nonassoc NOT

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                                               { NumS $1 }
  | TRUE                                                { BoolS true}
  | FALSE                                               { BoolS false}
  | IF expr THEN expr ELSE expr            { IfS ($2, $4, $6) }
  | expr OR expr                                    { IfS ($1, BoolS true, (IfS ($3, BoolS true, BoolS false))) }
  | expr AND expr                                 { IfS ($1, (IfS ($3, BoolS true, BoolS false)), BoolS false) }
  | NOT expr                                            { IfS ($2, BoolS false, BoolS true) }
;

