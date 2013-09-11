type unary_op = Unary_minus | Not
type binary_op = Plus | Minus | Mult | And | Or

type expr =
| Var of string
| Int of int
| Bool of bool
| String of string
| Unary_op of unary_op * expr
| Binary_op of binary_op * expr * expr
| List of expr list
| Tuple of expr list
| Record of (string * expr) list
