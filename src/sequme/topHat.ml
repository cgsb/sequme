open Batteries_uni;; open Printf

exception Error of string

type cmd = {
  exec : string;
  min_anchor_length : int option;
  solexa1_3_quals : bool;
  num_threads : int option;
  max_multihits : int option;
  no_coverage_search : bool;
  coverage_search : bool;
  butterfly_search : bool;
  gtf : string option;
  no_novel_juncs : bool;
  output_dir : string option;
  index_base : string;
  reads1 : string list;
  reads2 : string list;
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
    ?gtf
    ?(no_novel_juncs=false)
    ?output_dir
    index_base reads1 reads2
    =
  if List.length reads1 = 0 then
    Error "must provide input reads1" |> raise
  else {
    exec;
    min_anchor_length;
    solexa1_3_quals;
    num_threads;
    max_multihits;
    no_coverage_search;
    coverage_search;
    butterfly_search;
    gtf;
    no_novel_juncs;
    output_dir;
    index_base;
    reads1;
    reads2
  }

let cmd_to_string cmd = List.fold_left (^) "" [
  cmd.exec;
  (match cmd.min_anchor_length with None -> "" | Some x -> sprintf " -a %d" x);
  if cmd.solexa1_3_quals then " --solexa1.3-quals" else "";
  (match cmd.num_threads with None -> "" | Some x -> sprintf " -p %d" x);
  (match cmd.max_multihits with None -> "" | Some x -> sprintf " -g %d" x);
  if cmd.no_coverage_search then " --no-coverage-search" else "";
  if cmd.coverage_search then " --coverage-search" else "";
  if cmd.butterfly_search then " --butterfly-search" else "";
  (match cmd.gtf with None -> "" | Some x -> sprintf " -G %s" x);
  if cmd.no_novel_juncs then " --no-novel-juncs" else "";
  (match cmd.output_dir with None -> "" | Some x -> sprintf " -o %s" x);
  sprintf " %s" cmd.index_base;
  sprintf " %s" (String.concat "," cmd.reads1);
  if List.length cmd.reads2 > 0 then sprintf " %s" (String.concat "," cmd.reads2) else ""
]
