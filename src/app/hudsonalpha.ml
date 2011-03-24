open Batteries_uni;; open Sequme;; open Printf
open HudsonAlpha

;;
try
  Sys.argv.(1) |> get_status_html |> get_marias_table |> get_sequenced_libids
with e ->
  print_endline (Printexc.to_string e);
  Printexc.print_backtrace Legacy.stderr;
  exit 1
