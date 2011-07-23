#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme
open Filename

let root = List.reduce concat ["/data"; "users"; "aa144"; "th17"]

let run treatment_sl_id control_sl_id =
  let exec = "/home/aa144/local/python/bin/macs14" in
  let name = sprintf "%s_%s" treatment_sl_id control_sl_id in
  let format = "sam" in
  let pvalue = "1e-10" in
  let mfold_low = 15l in
  let mfold_high = 30l in
  let tsize = 36l in
  let gsize = "mm" in
  let bw = 200l in
  let wig = false in
  let space = 1l in (* only relevant if wig = true *)

  let treatment_sam_file = List.reduce concat [root; "sam_keep_name_only"; treatment_sl_id ^ ".sam"] in
  let control_sam_file = List.reduce concat [root; "sam_keep_name_only"; control_sl_id ^ ".sam"] in

  let outdir = List.reduce concat
    [root; "macs"; treatment_sl_id ^ "_" ^ control_sl_id]
  in
  Unix.mkdir outdir 0o755;

  let macs_cmd = Macs.make_cmd ~exec
      ~name ~format ~pvalue ~mfold:(mfold_low,mfold_high)
      ~tsize ~gsize ~bw
      ~wig ~space
      ~control:control_sam_file ~treatment:treatment_sam_file () in

  let macs_outdir = concat outdir "macs_out" in
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
  let pbs_outdir = concat outdir "pbs_out" in
  Pbs.make_and_run ~job_name pbs_outdir cmds

;;

run Sys.argv.(1) Sys.argv.(2)
