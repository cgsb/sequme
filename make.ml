open Printf
open Sys

let usage = sprintf "%s (setup | setup-clean)" argv.(0)

let command x = ignore (command x)

let remove file = if file_exists file then remove file else ()

let rec run = function
  | "setup" -> (
      run "setup-clean";
      command "oasis setup";
      command "cat ocamlbuild_ocamldoc.ml >> myocamlbuild.ml";
      command "cat custom_tags >> _tags"
    )
  | "setup-clean" -> (
      command "oasis setup-clean";
      remove "myocamlbuild.ml";
      remove "_tags";
      remove "Makefile";
      remove "configure";
      remove "setup.ml"
    )
  | _ -> eprintf "%s\n" usage

let () = match Array.length argv with
  | 2 -> run argv.(1)
  | 0 -> assert false
  | _ -> eprintf "%s\n" usage
