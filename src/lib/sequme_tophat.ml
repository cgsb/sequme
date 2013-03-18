open Sequme_internal_pervasives

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

let cmd_to_string cmd =
  let s opt x = match x with None -> "" | Some x -> sprintf " -%c %s" opt x in
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %d" opt x in
  String.concat ~sep:"" [
    cmd.exec;
    i 'a' cmd.min_anchor_length;
    if cmd.solexa1_3_quals then " --solexa1.3-quals" else "";
    i 'p' cmd.num_threads;
    i 'g' cmd.max_multihits;
    if cmd.no_coverage_search then " --no-coverage-search" else "";
    if cmd.coverage_search then " --coverage-search" else "";
    if cmd.butterfly_search then " --butterfly-search" else "";
    s 'G' cmd.gtf;
    if cmd.no_novel_juncs then " --no-novel-juncs" else "";
    s 'o' cmd.output_dir;
    sprintf " %s" cmd.index_base;
    sprintf " %s" (String.concat  ~sep:"," cmd.reads1);
    if List.length cmd.reads2 > 0
    then sprintf " %s" (String.concat ~sep:"," cmd.reads2) else ""
]
