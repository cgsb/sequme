#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let root = List.reduce Filename.concat ["/data"; "users"; "aa144"; "th17"]

let run genome sl_id =
  let exec = "/share/apps/tophat/1.2.0/tophat-1.2.0.Linux_x86_64/tophat" in
  let min_anchor_length = 10 in
  let solexa1_3_quals = false in
  let num_threads = 6 in
  let max_multihits = 20 in
  let no_coverage_search = false in
  let coverage_search = false in
  let butterfly_search = false in
  let gtf = match genome with
    | "mm9" -> "/data/sequme/db/th17/cachedfile/refGeneAnnot.gtf"
    | "hg19" -> "/data/users/aa144/th17/refseq/hg19_refseq_annot_May_1_2011.gtf"
    | _ -> failwith (sprintf "unknown genome %s" genome)
  in
  let no_novel_juncs = true in
  let index_path = match genome with
    | "mm9" -> "/data/sequme/db/bowtie/indexes/mm9/mm9"
    | "hg19" -> "/data/sequme/db/bowtie/indexes/hg19/hg19"
    | _ -> failwith (sprintf "unknown genome %s" genome)
  in
  let fastq_file_path = List.reduce Filename.concat [root; "fastq"; sl_id ^ ".fastq"] in
  let outdir = List.reduce Filename.concat [root; "tophat"; sl_id] in
  let _ = Unix.mkdir outdir 0o755 in
  let tophat_outdir = Filename.concat outdir "tophat_out" in

  let cmd = TopHat.make_cmd ~exec
    ~min_anchor_length
    ~solexa1_3_quals
    ~num_threads
    ~max_multihits
    ~no_coverage_search
    ~coverage_search
    ~butterfly_search
    ~gtf
    ~no_novel_juncs
    ~output_dir:tophat_outdir
    index_path
    [fastq_file_path]
    []
  in
  
  let job_name = sprintf "tophat_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=8,mem=14gb" in
  let pbs_outdir = Filename.concat outdir "pbs_out" in
  let cmds = [
    TopHat.cmd_to_string cmd;
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in

  Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds

;;

run Sys.argv.(1) Sys.argv.(2)

