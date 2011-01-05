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

(** [to_string e] returns the string that can be typed directly on the
    command line to run tophat. *)
let cmd_string e = List.fold_left (^) "" [
  "tophat";
  (match t.min_anchor_length with None -> "" | Some x -> sprintf " -a %d" x);
  (if t.solexa1_3_quals then " --solexa1.3-quals" else "");
  (match t.num_threads with None -> "" | Some x -> sprintf " -p %d" x);
  (match t.max_multihits with None -> "" | Some x -> sprintf " -g %d" x);
  (if t.no_coverage_search then " --no-coverage-search" else "");
  (if t.coverage_search then " --coverage-search" else "");
  (if t.butterfly_search then " --butterfly-search" else "")
]
