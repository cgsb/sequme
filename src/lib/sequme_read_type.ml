open Sequme_internal_pervasives

exception Error of string

type t = SingleEnd | PairedEnd

let of_string x =
  let x' = String.map ~f:Char.lowercase x in
  if x' = "se" || String.is_prefix x ~prefix:"single" then
    SingleEnd
  else if x' = "pe" || String.is_prefix x ~prefix:"paired" then
    PairedEnd
  else
    Error (sprintf "unknown read type %s" x) |> raise
