#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let mkpath = List.fold_left Filename.concat

let run root meta sl_id =
  let m = List.filter (fun x -> x.Th17_meta.sl_id = sl_id) meta in
  assert (List.length m = 1);
  let m = List.hd m in

  let exec = "/share/apps/cufflinks/0.9.3/Linux_x86_64_precompiled/cufflinks" in
  let num_threads = 5 in
  let quartile_normalization = false in
  let gtf = match m.Th17_meta.organism with
    | "Mouse" -> mkpath root ["cachedfile"; "refGeneAnnot.gtf"]
    | "Human" -> mkpath root ["refseq"; "hg19_refseq_annot_May_1_2011.gtf"]
    | _ -> assert false
  in

  let outdir = mkpath root ["cufflinks"; sl_id] in
  Unix.mkdir outdir 0o755;

  let cufflinks_outdir = mkpath outdir ["cufflinks_out"] in

  let cmd = match m.Th17_meta.organism with
    | "Mouse" -> Cufflinks.make_cmd ~exec
        ~output_dir:cufflinks_outdir
          ~num_threads
          ~mask_file:(mkpath root ["cachedfile"; "20101217_rRNA_tRNA_mask.gtf"])
          ~quartile_normalization
          ~gtf
          (mkpath root ["tophat"; sl_id; "tophat_out"; "accepted_hits.bam"])
    | "Human" ->
        Cufflinks.make_cmd ~exec
          ~output_dir:cufflinks_outdir
          ~num_threads
          ~quartile_normalization
          ~gtf
          (mkpath root ["tophat"; sl_id; "tophat_out"; "accepted_hits.bam"])
    | _ -> assert false
  in

  let job_name = sprintf "cufflinks_%s" sl_id |> flip String.right 15 in
  let resource_list = "nodes=1:ppn=6" in
  let pbs_outdir = mkpath outdir ["pbs_out"] in
  let rename file = sprintf "mv %s %s"
    (mkpath cufflinks_outdir [file])
    (mkpath cufflinks_outdir [sl_id ^ "_" ^ file])
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

let root_dir = Sys.argv.(1)
let meta = mkpath root_dir ["metadata"; "metadata.tsv"] |> Th17_meta.of_file
let sl_id = Sys.argv.(2)
let _ = run root_dir meta sl_id
