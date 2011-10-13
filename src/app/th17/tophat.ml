#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let mkpath = List.fold_left Filename.concat

let run root meta sl_id =
  let m = List.filter (fun x -> x.Th17_meta.sl_id = sl_id) meta in
  assert (List.length m = 1);
  let m = List.hd m in

  let exec = "/share/apps/tophat/1.2.0/tophat-1.2.0.Linux_x86_64/tophat" in
  let min_anchor_length = 10 in

  let solexa1_3_quals = match m.Th17_meta.phred_offset with
    | "Q33" -> false
    | "Q64" -> true
    | _ -> assert false
  in

  let num_threads = 6 in
  let max_multihits = 20 in
  let no_coverage_search = false in
  let coverage_search = false in
  let butterfly_search = false in

  let gtf = match m.Th17_meta.organism with
    | "Mouse" -> mkpath root ["cachedfile"; "refGeneAnnot.gtf"]
    | "Human" -> mkpath root ["refseq"; "hg19_refseq_annot_May_1_2011.gtf"]
    | _ -> assert false
  in

  let no_novel_juncs = true in

  let index_path = match m.Th17_meta.organism with
    | "Mouse" -> mkpath root ["bowtie_index"; "mm9"; "mm9"]
    | "Human" -> mkpath root ["bowtie_index"; "hg19"; "hg19"]
    | _ -> assert false
  in

  let fastq_file_path = mkpath root ["fastq"; sl_id ^ ".fastq"] in
  let outdir = mkpath root ["tophat"; sl_id] in
  let _ = Unix.mkdir outdir 0o755 in
  let tophat_outdir = mkpath outdir ["tophat_out"] in

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
  let pbs_outdir = mkpath outdir ["pbs_out"] in
  let cmds = [
    TopHat.cmd_to_string cmd;
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in

  Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds

;;

let root_dir = Sys.argv.(1)
let meta = mkpath root_dir ["metadata"; "metadata.tsv"] |> Th17_meta.of_file
let sl_id = Sys.argv.(2)
let _ = run root_dir meta sl_id
