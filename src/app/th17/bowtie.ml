#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "biocaml"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let mkpath = List.fold_left Filename.concat

let run root meta sl_id =
  let m = List.filter (fun x -> x.Th17_meta.sl_id = sl_id) meta in
  assert (List.length m = 1);
  let m = List.hd m in

  let exec = "/share/apps/bowtie/0.12.7/gnu/bowtie" in
  let k = 1 in
  let best = true in
  let sam = true in
  let num_threads = 3 in
  let phred33_quals,phred64_quals = match m.Th17_meta.phred_offset with
    | "Q33" -> true,false
    | "Q64" -> false,true
    | _ -> assert false
  in

  let outdir = mkpath root ["bowtie"; sl_id] in
  let _ = Unix.mkdir outdir 0o755 in
  
  let ebwt = match m.Th17_meta.organism with
    | "Mouse" -> mkpath root ["bowtie_index"; "mm9"; "mm9"]
    | "Human" -> mkpath root ["bowtie_index"; "hg19"; "hg19"]
    | _ -> assert false
  in

  let hit_file = mkpath outdir [sl_id ^ ".sam"] in

  let cmd = Bowtie.make_cmd ~exec
    ~ebwt
    ~phred33_quals ~phred64_quals
    ~k
    ~best ~sam
    ~threads:num_threads
    ~hit:hit_file
    ~reads:(mkpath root ["fastq"; sl_id ^ ".fastq"])
  in

  let job_name = sprintf "bowtie_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=8,mem=14gb" in
  let pbs_outdir = mkpath outdir ["pbs_out"] in
  let cmds = [
    Bowtie.cmd_to_string cmd;
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
