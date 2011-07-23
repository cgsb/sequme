#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let root = List.reduce Filename.concat ["/data"; "users"; "aa144"; "th17"]

let run genome sl_id =
  let sam_file = List.reduce Filename.concat [root; "bowtie"; sl_id; sl_id ^ ".sam"] in
  let gtf_file = List.reduce Filename.concat
    [root; "refseq";
     match genome with
       | "mm9" -> "mm9_refseq_annot_May_1_2011.gtf"
       | "hg19" -> "hg19_refseq_annot_May_1_2011.gtf"
       | _ -> failwith (sprintf "unknown genome %s" genome)
    ]
  in
  let outdir = List.reduce Filename.concat [root; "htseq-count"; sl_id] in
  Unix.mkdir outdir 0o755;
  let out_file = Filename.concat outdir (sl_id ^ ".counts") in
  
  let cmd = sprintf "htseq-count --stranded=no --mode=union -q %s %s > %s" sam_file gtf_file out_file in

  let job_name = sprintf "htseq-count_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=8,mem=14gb" in
  let pbs_outdir = Filename.concat outdir "pbs_out" in
  let cmds = [
    cmd;
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in

  Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds

;;

run Sys.argv.(1) Sys.argv.(2)
