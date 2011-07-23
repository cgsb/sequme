#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let root = List.reduce Filename.concat ["/data"; "users"; "aa144"; "th17"]

let run genome sl_id =
  let exec = "/share/apps/bowtie/0.12.7/gnu/bowtie" in
  let k = 1 in
  let best = true in
  let sam = true in
  let num_threads = 3 in

  let outdir = List.reduce Filename.concat [root; "bowtie"; sl_id] in
  let _ = Unix.mkdir outdir 0o755 in
  
  let ebwt = match genome with
    | "mm9" -> "/data/sequme/db/bowtie/indexes/mm9/mm9"
    | "hg19" -> "/data/sequme/db/bowtie/indexes/hg19/hg19"
    | x -> failwith (sprintf "unknown genome %s" x)
  in

  let hit_file = Filename.concat outdir (sl_id ^ ".sam") in

  let cmd = Bowtie.make_cmd ~exec
    ~ebwt
    ~k
    ~best ~sam
    ~threads:num_threads
    ~hit:hit_file
    ~reads:(List.reduce Filename.concat [root; "fastq"; sl_id ^ ".fastq"])
  in

  let job_name = sprintf "bowtie_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=8,mem=14gb" in
  let pbs_outdir = Filename.concat outdir "pbs_out" in
  let cmds = [
    Bowtie.cmd_to_string cmd;
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in
  Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds

;;

module Set = Set.StringSet

let already_ran = Filename.concat root "bowtie" |> Sys.files_of |> Set.of_enum

let available_to_run = Filename.concat root "fastq" |> Sys.files_of
  |> Enum.filter (flip Filename.check_suffix ".fastq")
  |> Enum.map (flip Filename.chop_suffix ".fastq") |> Set.of_enum

let need_to_run = Set.diff available_to_run already_ran |> Set.enum |> List.of_enum

;;
let () = run Sys.argv.(1) Sys.argv.(2)
