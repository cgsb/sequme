#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "sequme"]
--
open Batteries_uni;; open Printf;; open Biocaml

let in_file = Sys.argv.(1)
let out_file = Sys.argv.(2)

let fix_name a =
  let f = function ' ' -> "_" | c -> String.of_char c in
  {a with Sam.qname = String.replace_chars f a.Sam.qname}

let outp = open_out ~mode:[`create; `excl; `text] ~perm:(File.unix_perm 0o644) out_file

let hdr,e = Sam.enum_file in_file
let _ =
  fprintf outp "%s\n" (Sam.header_to_string hdr);
  Enum.iter (fix_name |- Sam.alignment_to_string |- fprintf outp "%s\n") e;
  close_out outp
