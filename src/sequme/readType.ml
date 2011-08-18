open Batteries_uni;; open Printf

exception Error of string

type t = SingleEnd | PairedEnd

let of_string x =
  let x' = String.map Char.lowercase x in
  if x' = "se" || String.starts_with x "single" then
    SingleEnd
  else if x' = "pe" || String.starts_with x "paired" then
    PairedEnd
  else
    Error (sprintf "unknown read type %s" x) |> raise
