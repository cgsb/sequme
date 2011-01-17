open Batteries_uni;; open Printf

exception Error of string

type cmd = {
  exec : string;
  output_dir : string option;
  num_threads : int option;
  mask_file : string option;
  time_series : bool;
  quartile_normalization : bool;
  gtf : string;
  samples : string list list
}

let make_cmd
    ?(exec="cuffdiff")
    ?output_dir
    ?num_threads
    ?mask_file
    ?(time_series=false)
    ?(quartile_normalization=false)
    gtf samples
    =
  if List.length samples < 2 then
    Error "must supply at least two samples to cuffdiff" |> raise
  else {
    exec;
    output_dir;
    num_threads;
    mask_file;
    time_series;
    quartile_normalization;
    gtf;
    samples
  }

let cmd_to_string cmd =
  let s opt x = match x with None -> "" | Some x -> sprintf " -%c %s" opt x in
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %d" opt x in
  String.concat "" [
    cmd.exec;
    s 'o' cmd.output_dir;
    i 'p' cmd.num_threads;
    s 'M' cmd.mask_file;
    if cmd.time_series then " -T" else "";
    if cmd.quartile_normalization then " -N" else "";
    sprintf " %s" cmd.gtf;
    cmd.samples |> List.map (String.concat ",") |> String.concat " " |> sprintf " %s"
  ]
