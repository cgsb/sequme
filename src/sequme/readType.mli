(** Read types.*)
open Batteries_uni

exception Error of string

type t =
    | SingleEnd
    | PairedEnd

val of_string : string -> t
