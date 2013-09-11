%{
open Purel_ast
%}

%token <string> VAR
%token <int> INT
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS TIMES
%token NOT AND OR
%token COMMA SEMICOLON EQUAL
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES AND OR      /* medium precedence */
%nonassoc UMINUS NOT    /* highest precedence */

%type <Purel_ast.binary_op> binary_op
%type <Purel_ast.expr> main expr
%start main

%%

main:
| e = expr EOF
  { e }

expr:
| x=VAR
  { Var x }
| i=INT
  { Int i }
| TRUE
  { Bool true }
| FALSE
  { Bool false }
| x=STRING
  { String x }
| LPAREN e=expr RPAREN
  { e }
| e1=expr op=binary_op e2=expr
  { Binary_op (op, e1, e2) }
| MINUS e=expr %prec UMINUS
  { Unary_op (Unary_minus,e) }
| NOT e=expr
  { Unary_op (Not,e) }
| es=delimited(LBRACKET, separated_list(COMMA,expr), RBRACKET)
  { List es }
| es=delimited(LPAREN, separated_non_one_list(COMMA,expr), RPAREN)
  { Tuple es }
| l=delimited(LPAREN, separated_nonempty_list(COMMA,separated_pair(VAR,EQUAL,expr)), RPAREN)
  { Record l }

(* [separated_non_one_list(separator,X)] recognizes an empty list or a
   list of length 2 or greater, i.e. a list not of length 1. *)
%inline separated_non_one_list(separator,X):
| /* nothing */
  { [] }
| x=X; separator; xs=separated_nonempty_list(separator,X)
  { x::xs }

%inline binary_op:
| PLUS  {Plus}
| MINUS {Minus}
| TIMES {Mult}
| AND   {And}
| OR    {Or}
