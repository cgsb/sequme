#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme

let mkpath = List.fold_left Filename.concat

let run root meta treatment_sl_id =
  let m = List.filter (fun x -> x.Th17_meta.sl_id = treatment_sl_id) meta in
  assert (List.length m = 1);
  let m = List.hd m in

  let control_sl_id = m.Th17_meta.control_sl_id in

  let exec = "macs14" in
  let name = sprintf "%s_%s" treatment_sl_id control_sl_id in
  let format = "sam" in
  let pvalue = "1e-10" in
  let mfold_low = 15l in
  let mfold_high = 30l in
  let tsize = 36l in
  let gsize = "mm" in
  let bw = 200l in
  let wig = true in
  let space = 10l in (* only relevant if wig = true *)

  let treatment_sam_file = mkpath root ["sam_keep_name_only"; treatment_sl_id; treatment_sl_id ^ ".sam"] in
  let control_sam_file = mkpath root ["sam_keep_name_only"; control_sl_id; control_sl_id ^ ".sam"] in

  let outdir = mkpath root ["macs"; treatment_sl_id ^ "_" ^ control_sl_id] in
  Unix.mkdir outdir 0o755;

  let macs_cmd = Macs.make_cmd ~exec
      ~name ~format ~pvalue ~mfold:(mfold_low,mfold_high)
      ~tsize ~gsize ~bw
      ~wig ~space
      ~control:control_sam_file ~treatment:treatment_sam_file () in

  let macs_outdir = mkpath outdir ["macs_out"] in
  Unix.mkdir macs_outdir 0o755;

  let cmds = [
    sprintf "cd %s" macs_outdir;
    "";
    Macs.cmd_to_string macs_cmd;
    "";
    sprintf "find %s -type d -exec chmod 755 {} \\;" outdir;
    sprintf "find %s -type f -exec chmod 644 {} \\;" outdir;
  ]
  in

  let job_name = sprintf "macs_%s_%s" treatment_sl_id control_sl_id |> flip String.right 15 in
  let pbs_outdir = mkpath outdir ["pbs_out"] in
  Pbs.make_and_run ~job_name pbs_outdir cmds

;;

let root_dir = Sys.argv.(1)
let meta = mkpath root_dir ["metadata"; "metadata.tsv"] |> Th17_meta.of_file
let sl_id = Sys.argv.(2)
let _ = run root_dir meta sl_id
