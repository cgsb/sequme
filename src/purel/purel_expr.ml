open Purel_internal_pervasives

type unary_op = Purel_ast.unary_op =
| Unary_minus
| Not
with sexp

type binary_op = Purel_ast.binary_op =
| Plus
| Minus
| Mult
| And
| Or
with sexp

type t = Purel_ast.expr =
| Var of string
| Int of int
| Bool of bool
| String of string
| Unary_op of unary_op * t
| Binary_op of binary_op * t * t
| List of t list
| Tuple of t list
| Record of (string * t) list
with sexp

let of_string s =
  Purel_parser.main Purel_lexer.token (Lexing.from_string s)

let rec is_value = function
  | Var _ -> false
  | Int _
  | Bool _
  | String _ -> true
  | Unary_op _
  | Binary_op _ -> false
  | List ts
  | Tuple ts -> List.for_all ts ~f:is_value
  | Record l -> List.(for_all (map l ~f:snd) ~f:is_value)

let free_vars expr =
  let ans = ref String.Set.empty in
  let rec loop = function
    | Var x -> ans := Set.add !ans x
    | Int _
    | Bool _
    | String _ -> ()
    | Unary_op (_,x) -> loop x
    | Binary_op (_,s,t) -> (loop s; loop t)
    | List ts
    | Tuple ts -> List.iter ts ~f:loop
    | Record l -> List.(iter (map l ~f:snd) ~f:loop)
  in
  loop expr;
  !ans

module Infix = struct

  let var x = Var x
  let int x = Int x
  let bool x = Bool x
  let string x = String x
  let neg x = Unary_op (Unary_minus, x)
  let not x = Unary_op (Not, x)
  let (-) x y = Binary_op (Minus, x, y)
  let (+) x y = Binary_op (Plus, x, y)
  let ( * ) x y = Binary_op (Mult, x, y)
  let ( || ) x y = Binary_op (And, x, y)
  let ( && ) x y = Binary_op (Or, x, y)

end
