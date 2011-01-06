(** Tophat support. *)

open Batteries_uni;; open Printf

(** Tophat command. *)
type cmd = {
  exec : string;
  min_anchor_length : int option;
  solexa1_3_quals : bool;
  num_threads : int option;
  max_multihits : int option;
  no_coverage_search : bool;
  coverage_search : bool;
  butterfly_search : bool;
}

let make_cmd
    ?(exec="tophat")
    ?min_anchor_length
    ?(solexa1_3_quals=false)
    ?num_threads
    ?max_multihits
    ?(no_coverage_search=false)
    ?(coverage_search=false)
    ?(butterfly_search=false)
    ()
    =
  {
    exec;
    min_anchor_length;
    solexa1_3_quals;
    num_threads;
    max_multihits;
    no_coverage_search;
    coverage_search;
    butterfly_search
  }

(** [to_string cmd] returns the string that can be typed directly on the
    command line to run tophat. *)
let cmd_string cmd = List.fold_left (^) "" [
  cmd.exec;
  (match cmd.min_anchor_length with None -> "" | Some x -> sprintf " -a %d" x);
  (if cmd.solexa1_3_quals then " --solexa1.3-quals" else "");
  (match cmd.num_threads with None -> "" | Some x -> sprintf " -p %d" x);
  (match cmd.max_multihits with None -> "" | Some x -> sprintf " -g %d" x);
  (if cmd.no_coverage_search then " --no-coverage-search" else "");
  (if cmd.coverage_search then " --coverage-search" else "");
  (if cmd.butterfly_search then " --butterfly-search" else "")
]
