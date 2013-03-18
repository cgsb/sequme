open Sequme_internal_pervasives

exception Error of string

type cmd = {
  exec : string;
  output_dir : string option;
  num_threads : int option;
  mask_file : string option;
  quartile_normalization : bool;
  gtf : string option;
  aligned_reads : string
}

let make_cmd
    ?(exec="cufflinks")
    ?output_dir
    ?num_threads
    ?mask_file
    ?(quartile_normalization=false)
    ?gtf
    aligned_reads
    =
  {
    exec;
    output_dir;
    num_threads;
    mask_file;
    quartile_normalization;
    gtf;
    aligned_reads
  }

let cmd_to_string cmd =
  let s opt x = match x with None -> "" | Some x -> sprintf " -%c %s" opt x in
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %d" opt x in
  String.concat ~sep:"" [
    cmd.exec;
    s 'o' cmd.output_dir;
    i 'p' cmd.num_threads;
    s 'M' cmd.mask_file;
    if cmd.quartile_normalization then " -N" else "";
    s 'G' cmd.gtf;
    sprintf " %s" cmd.aligned_reads
  ]
