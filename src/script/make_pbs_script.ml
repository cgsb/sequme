#!/usr/bin/env ocamlscript
Ocaml.ocamlflags := [ "-thread"];;
Ocaml.packs := [ "batteries";  ];;
--
open Batteries
open Printf

module System = struct
  let env_var s = 
    try Some (Sys.getenv s) 
    with Not_found -> None

  let command s = ignore (Unix.system s)

  let command_to_string s =
    BatUnix.open_process_in s |> IO.read_all

end


let () =
  let email = 
    ref (sprintf "%s@nyu.edu" 
           (Option.default "NOTSET" (System.env_var "LOGNAME"))) in
  let queue = 
    try 
      let groups =
        System.command_to_string "groups"in
      String.find groups "cgsb" |> ignore;
      ref (Some "cgsb-s")
    with
      e -> ref None in
  let template = ref None in
  let variables = ref [] in
  let root = ref (Option.default "NOTSET" (System.env_var "PWD")) in
  let nodes = ref 0 in
  let ppn = ref 0 in
  let options = [
    ( "-email", 
      Arg.Set_string email,
      sprintf "<address>\n\tSet the email (default, inferred: %s)." !email);
    ( "-queue", 
      Arg.String (fun s -> queue := Some s),
      sprintf "<name>\n\tSet the queue (default, inferred: %s)." (Option.default "None"  !queue));
    ( "-var", 
      Arg.String (fun s -> variables := s :: !variables),
      "<path>\n\tAdd an environment variable.");
    ( "-template", 
      Arg.String (fun s -> template := Some s),
      "<path>\n\tGive a template file.");
    ( "-root", 
      Arg.Set_string root,
      sprintf "<path>\n\tSet the root directory (default: %s)." !root);
    ( "-nodes-ppn", 
      Arg.Tuple [ Arg.Set_int nodes; Arg.Set_int ppn],
      "<n> <m>\n\tSet the number of nodes and processes per node.");
  ] in
  let names = ref [] in
  let anon s = names := s :: !names in
  let usage = sprintf "%s [OPTIONS] <scriptnames>" Sys.argv.(0) in
  Arg.parse options anon usage;

  List.iter (fun name ->
    let out_dir_name = sprintf "%s/%s" !root name in
    System.command  (sprintf "mkdir -p %s" out_dir_name); 
    let script = 
      sprintf "#!/bin/bash

#PBS -m abe
#PBS -M %s
#PBS -l %swalltime=12:00:00
#PBS -V
#PBS -o %s/%s.stdout
#PBS -e %s/%s.stderr
#PBS -N %s
%s

export NAME=%s
export OUT_DIR=%s/
%s
echo \"Script $NAME Starts on `date -R`\"

%s

echo \"Script $NAME Ends on `date -R`\"

" !email
(match !nodes, !ppn with 0, 0 -> "" | n, m -> sprintf "nodes=%d:ppn=%d," n m)
out_dir_name name
out_dir_name name
name
(match !queue with None -> "" | Some s -> sprintf "#PBS -q %s\n" s)
name
out_dir_name
(String.concat "\n" (List.map (sprintf "export %s") !variables))
(Option.map File.lines_of !template |> 
    Option.default (Enum.empty ()) |> 
    Enum.fold (fun a b -> sprintf "%s\n%s" a b) "# User script:")
    in
    File.with_file_out (sprintf "%s/script_%s.pbs" out_dir_name name) 
      (fun o ->
        fprintf o "%s" script);
  ) !names;

  ()
