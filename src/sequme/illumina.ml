open Batteries_uni;; open Printf

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


module DemultiplexStats = struct

  exception Error of string

  let int s =
    try int_of_string s
    with Failure _ -> Error (sprintf "%s is not an int" s) |> raise

  let float s =
    try float_of_string s
    with Failure _ -> Error (sprintf "%s is not a float" s) |> raise

  let qint =
    Util.unquote
    |- String.replace_chars (function ',' -> "" | c -> (sprintf "%c" c))
    |- int

  module Barcode = struct
    type t = {
      lane : int;
      sample_id : string;
      sample_ref : string;
      index : string;
      description : string;
      control : bool;
      project : string;
      yield : int;
      percent_pass_filter : float;
      num_reads : int;
      percent_of_raw_clusters_per_lane : float;
      percent_perfect_index_reads : float;
      percent_one_mismatch_reads : float;
      percent_gte_Q30 : float;
      mean_quality_score : float;
    }

    let of_string line = match String.nsplit line "\t" with
      | [lane;sample_id;sample_ref;index;description;
        control;project;yield;pass_filter;num_reads;
        percent_of_raw;percent_perfect;percent_one;
        percent_q30;mean_quality]
        ->
          {
            lane = int lane;
            sample_id;
            sample_ref;
            index;
            description;
            control = (match control with
              | "Y" -> true
              | "N" -> false
              | x -> raise (Error (sprintf "invalid value %s for control" x))
            );
            project;
            yield = qint yield;
            percent_pass_filter = float pass_filter;
            num_reads = qint num_reads;
            percent_of_raw_clusters_per_lane = float percent_of_raw;
            percent_perfect_index_reads = float percent_perfect;
            percent_one_mismatch_reads = float percent_one;
            percent_gte_Q30 = float percent_q30;
            mean_quality_score = float mean_quality
          }
      | x -> Error (sprintf "expected exactly 15 columns but found %d" (List.length x)) |> raise

  end

  module Sample = struct
    type t = {
      sample_id : string;
      recipe : string;
      operator : string;
      directory : string
    }

    let of_string line = match String.nsplit line "\t" with
      | [sample_id;recipe;operator;directory;_;_;_;_;_;_;_;_;_;_;_] ->
          {sample_id;recipe;operator;directory}
      | x -> Error (sprintf "expected exactly 15 columns but found %d" (List.length x)) |> raise

  end

  type flowcell = string
  type software = string
 
  type t = flowcell * Barcode.t list * Sample.t list * software

  let of_rendered_html file =
    let e = File.lines_of file in
    let flowcell = Enum.get e |> Option.get |> flip String.split "Flowcell: " |> snd |> String.trim in
    Enum.iter (fun _ -> Enum.junk e) (1--4);
    let barcodes =
      e
      |> Enum.take_while (not -| flip String.starts_with "Sample information")
      |> Enum.fold (fun ans line -> (Barcode.of_string line)::ans) []
      |> List.rev
    in
    Enum.iter (fun _ -> Enum.junk e) (1--4);
    let samples =
      e
      |> Enum.take_while (not -| flip String.starts_with "CASAVA")
      |> Enum.fold (fun ans line -> (Sample.of_string line)::ans) []
      |> List.rev
    in
    let software = Enum.get e |> Option.get |> String.trim in
    flowcell,barcodes,samples,software

end
