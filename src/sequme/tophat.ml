(** Tophat support. *)

open Batteries_uni;; open Printf

(** Tophat expression. *)
type expr = {
  min_anchor_length : int option;
  solexa1_3_quals : bool;
  num_threads : int option;
  max_multihits : int option;
  no_coverage_search : bool;
  coverage_search : bool;
  butterfly_search : bool;
}

let make_expr
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
    min_anchor_length;
    solexa1_3_quals;
    num_threads;
    max_multihits;
    no_coverage_search;
    coverage_search;
    butterfly_search
  }

(** [to_string e] returns the string that can be typed directly on the
    command line to run tophat. *)
let cmd_string e = List.fold_left (^) "" [
  "tophat";
  (match e.min_anchor_length with None -> "" | Some x -> sprintf " -a %d" x);
  (if e.solexa1_3_quals then " --solexa1.3-quals" else "");
  (match e.num_threads with None -> "" | Some x -> sprintf " -p %d" x);
  (match e.max_multihits with None -> "" | Some x -> sprintf " -g %d" x);
  (if e.no_coverage_search then " --no-coverage-search" else "");
  (if e.coverage_search then " --coverage-search" else "");
  (if e.butterfly_search then " --butterfly-search" else "")
]
