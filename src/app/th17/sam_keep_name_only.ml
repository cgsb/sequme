#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Biocaml
open Filename

let root = List.reduce concat ["/data"; "users"; "aa144"; "th17"]

let run sl_id =
  let in_file = List.reduce concat [root; "bowtie"; sl_id; sl_id ^ ".sam"] in
  let out_file = List.reduce concat [root; "sam_keep_name_only"; sl_id ^ ".sam"] in
  let outp = open_out ~mode:[`create; `excl; `text] ~perm:(File.unix_perm 0o644) out_file in
  let hdr,e = Sam.enum_file in_file in
  fprintf outp "%s\n" (Sam.header_to_string hdr);
  let f a = {a with Sam.qname = String.replace_chars (function ' ' -> "_" | c -> String.of_char c) a.Sam.qname} in
  Enum.iter (f |- Sam.alignment_to_string |- fprintf outp "%s\n") e

;;
run Sys.argv.(1)
