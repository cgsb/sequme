#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme
open Filename

let root = List.reduce concat ["/data"; "users"; "aa144"; "th17"]

let run genome sl_id =
  let exec = "/share/apps/cufflinks/0.9.3/Linux_x86_64_precompiled/cufflinks" in
  let num_threads = 5 in
  let quartile_normalization = false in
  let gtf = match genome with
    | "mm9" -> "/data/sequme/db/th17/cachedfile/refGeneAnnot.gtf"
    | "hg19" -> "/data/users/aa144/th17/refseq/hg19_refseq_annot_May_1_2011.gtf"
    | _ -> failwith (sprintf "unknown genome %s" genome)
  in

  let outdir = List.reduce concat [root; "cufflinks"; sl_id] in
  Unix.mkdir outdir 0o755;

  let cufflinks_outdir = concat outdir "cufflinks_out" in

  let cmd = match genome with
    | "mm9" -> Cufflinks.make_cmd ~exec
        ~output_dir:cufflinks_outdir
          ~num_threads
          ~mask_file:"/data/sequme/db/th17/cachedfile/20101217_rRNA_tRNA_mask.gtf"
          ~quartile_normalization
          ~gtf
          (List.reduce concat [root; "tophat"; sl_id; "tophat_out"; "accepted_hits.bam"])
    | "hg19" ->
        Cufflinks.make_cmd ~exec
          ~output_dir:cufflinks_outdir
          ~num_threads
          ~quartile_normalization
          ~gtf
          (List.reduce concat [root; "tophat"; sl_id; "tophat_out"; "accepted_hits.bam"])
    | _ -> failwith (sprintf "unknown genome %s" genome)
  in

  let job_name = sprintf "cufflinks_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=6" in
  let pbs_outdir = concat outdir "pbs_out" in
  let rename file = sprintf "mv %s %s"
    (concat cufflinks_outdir file)
    (concat cufflinks_outdir (sl_id ^ "_" ^ file))
  in
  let cmds = [
    Cufflinks.cmd_to_string cmd;
    "";
    rename "genes.expr";
    rename "transcripts.expr";
    rename "transcripts.gtf";
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in
  
  Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds

;;

run Sys.argv.(1) Sys.argv.(2)
