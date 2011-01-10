(** Utility functions. A place to quickly place functions that don't
    obviously fit into another module, or don't justify defining a new
    module. *)

open Batteries_uni;; open Printf

(** [unquote ~quote x] unquotes string [x] treating [quote] as the
    quotation character (default = '\"'. Returns original string, not
    a copy, if length of [x] < 2 or if first and last character of [x]
    are not [quote]. *)
let unquote ?(quote='\"') (x : string) : string =
  let n = String.length x in
  if n < 2 then x
  else if x.[0] = '\"' && x.[n-1] = '\"' then
    String.slice ~first:1 ~last:(n-1) x
  else x

(** [tm_to_string tm] prints [tm] in the format "YYYY-MM-DD
    HH-MM-SS". *)
let tm_to_string (tm : Unix.tm) : string =
  let open Unix in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + tm.tm_year) (1+tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
