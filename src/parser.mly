%{
  open Ast
%}

%token FORALL FUN TYPE
%token NAT SUCC ELIMNAT
%token <int> NUMERAL
%token <string> NAME
%token LPAREN RPAREN
%token COLON COMMA PERIOD COLONEQUAL
%token ARROW DARROW
%token QUIT HELP PARAMETER CHECK EVAL CONTEXT DEFINITION
%token EOF

%start <Ast.directive list> directives

%%

(* Toplevel syntax *)

directives:
  | dir = directive PERIOD EOF
     { [dir] }
  | dir = directive PERIOD lst = directives
     { dir :: lst }

directive:
  | QUIT
    { Quit }
  | HELP
    { Help }
  | x = NAME COLON e = expr
    { Parameter (String x, e) }
  | x = NAME COLONEQUAL e = expr
    { Definition (String x, e) }
  | CHECK e = expr
    { Check e }
  | EVAL e = expr
    { Eval e}
  | CONTEXT
    { Context }

(* Main syntax tree *)

expr:
  | e = app_expr
    { e }
  | FORALL a = pi_abstraction
    { Pi a }
  | LPAREN x = NAME COLON t = expr RPAREN ARROW t2 = expr
    { Pi (String x, t, t2) }
  | t1 = app_expr ARROW t2 = expr
    { Pi (Dummy, t1, t2) }
  | FUN a = fun_abstraction
    { Lambda a }
  | ELIMNAT e1 = app_expr e2 = app_expr e3 = app_expr e4 = app_expr
    { ElimNat (e1, e2, e3, e4) }
  | SUCC e = expr
    { Succ e }

app_expr:
  | e = simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr:
  | n = NAME
    { Var (String n) }
  | TYPE k = NUMERAL
    { Universe k }
  | LPAREN e = expr RPAREN
    { e }
  | k = NUMERAL
    { assert (k = 0) ; Zero }
  | NAT
    { Nat }

pi_abstraction:
  | LPAREN x = NAME COLON t = expr RPAREN COMMA e = expr
    { (String x, t, e) }
  | x = NAME COLON t = expr COMMA e = expr
    { (String x, t, e) }

fun_abstraction:
  | x = NAME COLON t = expr DARROW e = expr
    { (String x, t, e) }

%%
