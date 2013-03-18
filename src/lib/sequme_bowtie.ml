open Sequme_internal_pervasives

exception Error of string

type cmd = {
  exec : string;
  ebwt : string;
  phred33_quals : bool;
  phred64_quals : bool;
  k : int option;
  best : bool;
  sam : bool;
  threads : int option;
  reads : string;
  hit : string option;
}

let make_cmd
    ?(exec="bowtie") ~ebwt
    ?(phred33_quals=false) ?(phred64_quals=false)
    ?k ?(best=false) ?(sam=false) ?threads
    ?hit ~reads
    =
  {
    exec;
    ebwt;
    phred33_quals;
    phred64_quals;
    k;
    best;
    sam;
    threads;
    reads;
    hit
  }

let cmd_to_string cmd =
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %d" opt x in
  String.concat ~sep:"" [
    cmd.exec;

    (* options *)
    i 'k' cmd.k;
    if cmd.best then " --best" else "";
    if cmd.sam then " --sam" else "";
    if cmd.phred33_quals then " --phred33-quals" else "";
    if cmd.phred64_quals then " --phred64-quals" else "";
    i 'p' cmd.threads;

    (* ebwt, reads, hit *)
    sprintf " %s" cmd.ebwt;
    sprintf " %s" cmd.reads;
    (match cmd.hit with Some hit -> sprintf " %s" hit | None -> "")
  ]
