open Sequme_internal_pervasives

(** Like [int_of_string] but with a better error message. *)
let int s =
  try Int.of_string s
  with _ -> failwith (sprintf "%s is not an int" s)

(** Like [float_of_string] but with a better error message. *)
let float s =
  try Float.of_string s
  with _ -> failwith (sprintf "%s is not a float" s)

let control_of_string = function
  | "Y" -> true
  | "N" -> false
  | x -> failwith (sprintf "control must be Y or N but is %s" x)



module Tile = struct
  exception Error of string
  type surface = Top | Bottom
  type t = {surface:surface; swath:int; tile_num:int;}

  let error s = raise (Error s)

  let of_string_exn s =
    if String.length s <> 4
      || not (String.for_all s ~f:Char.is_digit)
    then
      error (sprintf "invalid tile %s" s)
    else
      let surface = match s.[0] with
        | '1' -> Top
        | '2' -> Bottom
        | x -> error (sprintf "invalid surface %c" x)
      in
      let swath = match s.[1] with
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | x -> error (sprintf "invalid swath %c" x)
      in
      let tile_num =
        let s = String.(sub s ~pos:2 ~len:(length s - 2)) in
        let x = Int.of_string s in
        if x <= 0 then
          error (sprintf "invalid tile number %s" s)
        else
          x
      in
      {surface; swath; tile_num}

  let to_string t =
    sprintf "%c%d%02d"
      (match t.surface with Top -> '1' | Bottom -> '2')
      t.swath
      t.tile_num

end

module Fastq = struct
  exception Error of string

  type sequence_id = {
    instrument : string;
    run_number : int;
    flowcell_id : string;
    lane : int;
    tile : Tile.t;
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

    if not (String.is_prefix str ~prefix:"@") then
      raise (Error "sequence identifier must start with '@' character")
    else
      match String.lsplit2 str ~on:' ' with
      | Some (x, y) ->
        begin match String.split x ~on:':', String.split y ~on:':' with
        | [instrument;run_number;flowcell_id;lane;tile;x_pos;y_pos],
          [read;is_filtered;control_number;index] -> {
            instrument = String.slice instrument 1 0;
            run_number = i "run_number" run_number;
            flowcell_id;
            lane = i "lane" lane;
            tile = Tile.of_string_exn tile;
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

  type t = string

  let of_int x =
    try List.Assoc.find_exn code_seqs_list x
    with Not_found -> raise (Error (sprintf "invalid index %d" x))

  let of_ad_code x =
    try Scanf.sscanf x "AD%d" ident |> of_int
    with _ -> raise (Error (sprintf "invalid index %s" x))

  let of_seq x =
    if List.exists code_seqs_list (fun (_, s) -> s = x) then x
    else Error (sprintf "invalid index %s" x) |> raise

  let to_ad_code t =
    sprintf "AD%03d"
      (List.find_exn code_seqs_list ~f:(fun (x, s) -> t = s) |> fst)

  let to_seq t = t

end


module SampleSheet = struct
  exception Error of string

  type record = {
    flowcell_id : string;
    lane : int;
    sample_id : string;
    sample_ref : string;
    barcode : string;
    description : string;
    control : bool;
    recipe : string;
    operator : string;
    project : string
  }

  type t = record list

  let record_of_string x = match String.split x ~on:',' with
    | [flowcell_id;lane;sample_id;sample_ref;barcode;
       description;control;recipe;operator;project]
      ->
        {
          flowcell_id; lane = int lane; sample_id;
          sample_ref;
          barcode;
          description;
          control = control_of_string control;
          recipe; operator; project
        }
    | x -> Error (sprintf "expected exactly 10 columns but found %d" (List.length x)) |> raise

  let of_file file =
    In_channel.with_file file ~f:(fun chan ->
      let e = In_channel.input_lines chan in
      let default_header =
        "FCID,Lane,SampleID,SampleRef,Index,Description,\
         Control,Recipe,Operator,SampleProject" in
      match e with
      | hdr :: rest when default_header = hdr ->
        List.map rest record_of_string
      | hdr :: _ ->
        raise (Error (sprintf "SampleSheet: wrong header: %S" hdr))
      | [] ->
        raise (Error "SampleSheet: empty file")
    )

  let group_by_sample_id t =
    t
    |> List.fold_left ~f:(fun map x ->
          let prev =
            try String.Map.find_exn map x.sample_id
            with Not_found -> []
          in
          String.Map.add map x.sample_id (x::prev)
        ) ~init:String.Map.empty
    |> String.Map.map ~f:List.rev

  let find_lane_barcode t lane barcode =
    let rec loop = function
      | [] -> None
      | x::rest ->
          if x.lane = lane && x.barcode = barcode then Some x
          else loop rest
    in
    loop t

end


