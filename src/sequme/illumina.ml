open Batteries_uni;; open Biocaml;; open Printf

module Fastq = struct
  exception Error of string

  type sequence_id = {
    instrument : string;
    run_number : int;
    flowcell_id : string;
    lane : int;
    tile : int;
    x_pos : int;
    y_pos : int;
    read : int;
    is_filtered : bool;
    control_number : int;
    index : string
  }

  let sequence_id_of_string str =
    let f convert name value =
      try convert value
      with Failure _ -> raise (Error (sprintf "%s is not a valid %s" value name))
    in
    let i = f int_of_string in
    let b = f (function "Y" -> true | "N" -> false | _ -> failwith "") in

    if not (String.starts_with str "@") then
      raise (Error "sequence identifier must start with '@' character")
    else
      let x,y = String.split str " " in
      match String.nsplit x ":", String.nsplit y ":" with
        | [instrument;run_number;flowcell_id;lane;tile;x_pos;y_pos],
          [read;is_filtered;control_number;index]
          -> {
            instrument = String.lchop instrument;
            run_number = i "run_number" run_number;
            flowcell_id;
            lane = i "lane" lane;
            tile = i "tile" tile;
            x_pos = i "x_pos" x_pos;
            y_pos = i "y_pos" y_pos;
            read = i "read" read;
            is_filtered = b "is_filtered" is_filtered;
            control_number = i "control_number" control_number;
            index
          }
        | _ -> raise (Error ("invalid sequence identifier for Illumina Fastq file"))

end
