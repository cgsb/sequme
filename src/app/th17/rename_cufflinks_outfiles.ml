#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Sequme
open Filename

let root = List.reduce concat ["/data"; "users"; "aa144"; "th17"]

let exec cmd =
  print_endline cmd
  ; cmd |> Sys.command |> ignore
    
let run sl_id =
  let dir = List.reduce concat [root; "cufflinks"; sl_id; "cufflinks_out"] in
  sprintf "mv %s/genes.expr %s/%s_genes.expr" dir dir sl_id |> exec;
  sprintf "mv %s/transcripts.expr %s/%s_transcripts.expr" dir dir sl_id |> exec;
  sprintf "mv %s/transcripts.gtf %s/%s_transcripts.gtf" dir dir sl_id |> exec

;;

run Sys.argv.(1)
