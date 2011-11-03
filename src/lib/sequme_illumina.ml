open Sequme_std

module StringMap = Map.StringMap

(** Like [int_of_string] but with a better error message. *)
let int s =
  try int_of_string s
  with Failure _ -> failwith (sprintf "%s is not an int" s)

(** Like [float_of_string] but with a better error message. *)      
let float s =
  try float_of_string s
  with Failure _ -> failwith (sprintf "%s is not a float" s)

let control_of_string = function
  | "Y" -> true
  | "N" -> false
  | x -> failwith (sprintf "control must be Y or N but is %s" x)


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
      match String.split str " " with
      | Some (x, y) ->
        begin match String.nsplit x ":", String.nsplit y ":" with
        | [instrument;run_number;flowcell_id;lane;tile;x_pos;y_pos],
          [read;is_filtered;control_number;index] -> {
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
      | _ -> raise (Error ("invalid sequence identifier for Illumina Fastq file"))

end


module Barcode = struct
  exception Error of string

  module IntMap = Map.IntMap
  module StringMap = Map.StringMap
  module S = Set.Make(String)

  let code_seqs_list =
    [
      1, "ATCACG";
      2, "CGATGT";
      3, "TTAGGC";
      4, "TGACCA";
      5, "ACAGTG";
      6, "GCCAAT";
      7, "CAGATC";
      8, "ACTTGA";
      9, "GATCAG";
      10, "TAGCTT";
      11, "GGCTAC";
      12, "CTTGTA"
    ]

  (* Different representations of the same barcodes. *)
  let code_seqs = code_seqs_list |> List.enum |> IntMap.of_enum
  let seq_codes = code_seqs_list |> List.enum |> Enum.map ~f:(fun (x,y) -> y,x) |> StringMap.of_enum
  let barcodes = code_seqs_list |> List.enum |> Enum.map ~f:snd |> S.of_enum

  type t = string

  let of_int x =
    try IntMap.find x code_seqs
    with Not_found -> Error (sprintf "invalid index %d" x) |> raise

  let of_ad_code x =
    try String.split_exn x "AD" |> snd |> int_of_string |> of_int
    with Failure _ | Error _ -> Error (sprintf "invalid index %s" x) |> raise

  let of_seq x =
    if S.mem x barcodes then x
    else Error (sprintf "invalid index %s" x) |> raise

  let to_ad_code t =
    sprintf "AD%03d" (StringMap.find t seq_codes)

  let to_seq t = t

end


module SampleSheet = struct
  exception Error of string

  type record = {
    flowcell_id : string;
    lane : int;
    sample_id : string;
    sample_ref : string;
    barcode : Barcode.t;
    description : string;
    control : bool;
    recipe : string;
    operator : string;
    project : string
  }

  type t = record list

  let record_of_string x = match String.nsplit x "," with
    | [flowcell_id;lane;sample_id;sample_ref;barcode;
       description;control;recipe;operator;project]
      ->
        {
          flowcell_id; lane = int lane; sample_id;
          sample_ref;
          barcode = Barcode.of_seq barcode;
          description;
          control = control_of_string control;
          recipe; operator; project
        }
    | x -> Error (sprintf "expected exactly 10 columns but found %d" (List.length x)) |> raise
          
  let of_file file =
    let e = File.lines_of file in
    let hdr = Enum.get e |> Option.get in
    if not (hdr = "FCID,Lane,SampleID,SampleRef,Index,Description,Control,Recipe,Operator,SampleProject") then
      Error "invalid header" |> raise
    ;
    Enum.fold ~f:(fun ans line -> (record_of_string line)::ans) ~init:[] e |> List.rev


  let group_by_sample_id t =
    t 
    |> List.fold_left ~f:(fun map x ->
          let prev =
            try StringMap.find x.sample_id map
            with Not_found -> []
          in
          StringMap.add x.sample_id (x::prev) map
        ) ~init:StringMap.empty
    |> StringMap.map List.rev

  let find_lane_barcode t lane barcode =
    let rec loop = function
      | [] -> None
      | x::rest ->
          if x.lane = lane && x.barcode = barcode then Some x
          else loop rest
    in
    loop t

end


module DemultiplexStats = struct

  exception Error of string

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
            control = control_of_string control;
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
    let flowcell = Enum.get e |> Option.get |> flip String.split_exn "Flowcell: " |> snd |> String.trim in
    Enum.iter (fun _ -> Enum.junk e) (1--4);
    let barcodes =
      e
      |> Enum.take_while ~f:(not -| flip String.starts_with "Sample information")
      |> Enum.fold ~f:(fun ans line -> (Barcode.of_string line)::ans) ~init:[]
      |> List.rev
    in
    Enum.iter (fun _ -> Enum.junk e) (1--4);
    let samples =
      e
      |> Enum.take_while ~f:(not -| flip String.starts_with "CASAVA")
      |> Enum.fold ~f:(fun ans line -> (Sample.of_string line)::ans) ~init:[]
      |> List.rev
    in
    let software = Enum.get e |> Option.get |> String.trim in
    flowcell,barcodes,samples,software

end
