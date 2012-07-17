(** Support for PostgreSQL's .pgpass files. *)

exception Error of string

(** A value [x] of type ['a star] can be either a regular value of type
    ['a], or Star which is interpreted as all values of type ['a] for the
    purposes of matching routines. *)
type 'a star =
    | Val of 'a
    | Star

(** Database connection parameters. *)
type record = private {
  hostname : string star;
  port : int star;
  database : string star;
  username : string star;
  password : string;
}

type t = record list

val of_file_exn : string -> t

val find : hostname:string -> port:int -> database:string -> username:string -> t -> record option
  (** [find hostname port database username t] returns the first
      record in [t] matching the given database parameters, or None of
      no match is found. *)
