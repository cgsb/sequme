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

val of_string : string -> t

(** True if the given expression is a value, i.e. fully reduced. *)
val is_value : t -> bool

val free_vars : t -> Core.Std.String.Set.t

module Infix : sig

  val var : string -> t
  val int : int -> t
  val bool : bool -> t
  val string : string -> t
  val neg : t -> t
  val not : t -> t
  val (+): t -> t -> t
  val (-) : t -> t -> t
  val ( * ): t -> t -> t
  val ( || ) : t -> t -> t
  val ( && ) : t -> t -> t

end
