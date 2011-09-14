#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Biocaml;; open Sequme

let run root meta sl_id =
  (* let m = List.filter (fun x -> x.Th17_meta.sl_id = sl_id) meta in *)
  (* assert (List.length m = 1); *)
  (* let m = List.hd m in *)

  let in_file = List.reduce Filename.concat [root; "bowtie"; sl_id; sl_id ^ ".sam"] in

  let outdir = List.reduce Filename.concat [root; "sam_keep_name_only"; sl_id] in
  Unix.mkdir outdir 0o755;
  let out_file = Filename.concat outdir (sl_id ^ ".sam") in

  let exec = "/home/aa144/code/sequme/src/app/th17/fix_sam_name.ml" in
  let cmd = sprintf "%s %s %s" exec in_file out_file in

  let job_name = sprintf "sam_name_%s" sl_id |> flip String.right 15 in
  let resource_list = "mem=4gb" in
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
let root_dir = Sys.argv.(1)
let meta = List.reduce Filename.concat [root_dir; "metadata"; "metadata.tsv"] |> Th17_meta.of_file

let sl_ids =
  let open Th17_meta in
  meta
  |> List.filter_map (fun x ->
       if x.read_type = "SE" && x.application = "ChIP-Seq"
       then Some x.sl_id
       else None
     )

let _ = List.iter (print_endline) sl_ids
