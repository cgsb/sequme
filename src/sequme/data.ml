open Batteries_uni;; open Printf

module CachedFile = struct
  exception Error of string

  type db_record = {
    id : int32;
    owner_id : int32;
    added_by_id : int32;
    created : PGOCaml.timestamptz;
    last_modified : PGOCaml.timestamptz;
    name : string;
    url : string;
    md5sum : string;
    note : string;
  }

  let db_record_of_id dbh id =
    match PGSQL(dbh)
      "SELECT id,owner_id,added_by_id,created,
              last_modified,name,url,md5sum,note
       FROM data_cachedfile WHERE id=$id"
    with
      | [] -> None
      | (id,owner_id,added_by_id,created,
        last_modified,name,url,md5sum,note)::[] ->
          Some {id;owner_id;added_by_id;created;
                last_modified;name;url;md5sum;note}
      | _ -> assert false

  let db_record_of_name_exn dbh name =
    match PGSQL(dbh)
      "SELECT id,owner_id,added_by_id,created,
              last_modified,name,url,md5sum,note
       FROM data_cachedfile WHERE name=$name"
    with
      | [] -> Error (sprintf "unknown cached file %s" name) |> raise
      | (id,owner_id,added_by_id,created,
        last_modified,name,url,md5sum,note)::[] ->
          {id;owner_id;added_by_id;created;
           last_modified;name;url;md5sum;note}
      | _ -> assert false

  let db_record_of_name dbh name =
    try Some (db_record_of_name_exn dbh name)
    with Error _ -> None

end

module Barcode = struct
  type db_record = {
    id : int32;
    barcode : string
  }

  (** [insert t] inserts barcode [t] into the database and returns the
      [id] of the new record. If [t] was already in the database,
      returns [id] of the existing record. *)
  let insert ~barcode dbh =
    PGOCaml.begin_work dbh;
    let ans = match
        PGSQL(dbh) "SELECT id FROM data_barcode WHERE barcode=$barcode"
      with
        | id::[] -> id
        | [] -> PGSQL(dbh)
            "INSERT INTO data_barcode (barcode) VALUES ($barcode)";
            PGOCaml.serial4 dbh "data_barcode_id_seq"
        | _ -> assert false
    in
    PGOCaml.commit dbh;
    ans
end

module Sample = struct
  exception Error of string

  type db_record = {
    id : int32;
    owner_id : int32;
    made_by_id : int32;
    created : PGOCaml.timestamptz;
    last_modified : PGOCaml.timestamptz;
    name : string;
    note : string;
    exp_type : string;
    ref_genome_id : int32 option;
    cell_line : string;
    conditions : string;
    factor : string;
    antibody : string;
    concentration : float;
    mean_fragment_size : int32;
    barcoded : bool;
  }

  let insert ?owner ~made_by ?created ~name ~note ~exp_type
      ?ref_genome_name ~cell_line ~conditions ~factor ~antibody ?concentration
      ~mean_fragment_size ?barcodes dbh
      =
    let now = CalendarLib.Calendar.now(), CalendarLib.Time_Zone.current() in
    let created = Option.default now created in
    let last_modified = now in

    let barcodes = Option.default [] barcodes in
    let barcoded = List.length barcodes > 0 in

    PGOCaml.begin_work dbh;

    let user_id x = (Auth.User.of_username_exn dbh x).Auth.User.id in
    let made_by_id = user_id made_by in
    let owner_id = Option.map_default user_id made_by_id owner in

    let ref_genome_id = match ref_genome_name with
      | None -> None
      | Some x -> Some (CachedFile.db_record_of_name_exn dbh x).CachedFile.id
    in

    PGSQL(dbh)
      "INSERT INTO data_sample
       (owner_id,made_by_id,created,last_modified,
        name,note,exp_type,ref_genome_id,cell_line,
        conditions,factor,antibody,concentration,
        mean_fragment_size,barcoded)
       VALUES
       ($owner_id,$made_by_id,$created,$last_modified,
        $name,$note,$exp_type,$?ref_genome_id,$cell_line,
        $conditions,$factor,$antibody,$?concentration,
        $mean_fragment_size,$barcoded)"
    ;

    let sample_id = PGOCaml.serial4 dbh "data_sample_id_seq" in

    let add_barcode barcode =
      let barcode_id = Barcode.insert ~barcode dbh in
      PGSQL(dbh) "INSERT INTO data_sample_barcodes (sample_id,barcode_id) VALUES ($sample_id,$barcode_id)"
    in
    List.iter add_barcode barcodes;

    PGOCaml.commit dbh;
    sample_id

end

module IlluminaRun = struct
  exception Error of string

  type db_record = {
    id : int32;
    owner_id : int32;
    run_by_id : int32 option;
    run_at_id : int32;
    started : PGOCaml.timestamptz;
    finished : PGOCaml.timestamptz option;
    last_modified : PGOCaml.timestamptz;
    status : string;
    note : string;
  }

  type status = InProgress | Successful | Failed

  let status_to_string = function
    | InProgress -> "in_progress"
    | Successful -> "successful"
    | Failed -> "failed"

  let insert ?owner ?run_by ~run_at
      ?started ?finished ~status ?note
      ~samples dbh
      =
    let now = CalendarLib.Calendar.now(), CalendarLib.Time_Zone.current() in
    let last_modified = now in

    let finished,status = match finished,status with
      | None, _
      | Some _, (Successful|Failed) ->
          finished, status_to_string status
      | Some _, InProgress ->
          Error (sprintf "invalid to give finished time if status is %s" (status_to_string status)) |> raise
    in

    PGOCaml.begin_work dbh;

    let user_id x = (Auth.User.of_username_exn dbh x).Auth.User.id in
    let run_by_id = Option.map user_id run_by in
    let owner_id = match owner,run_by_id with
      | Some owner, _ -> user_id owner
      | None, Some run_by_id -> run_by_id
      | None, None -> Error (sprintf "must provide owner when run_by not provided") |> raise
    in
    let run_at_id = Profile.Institute.id_of_short_name_exn dbh run_at in

    let note = Option.default "" note in

    PGSQL(dbh)
      "INSERT INTO data_illuminarun
       (owner_id, run_by_id, run_at_id, started,
        finished, last_modified, status, note)
       VALUES
       ($owner_id, $?run_by_id, $run_at_id, $?started,
        $?finished, $last_modified, $status, $note)"
    ;

    let illuminarun_id = PGOCaml.serial4 dbh "data_illuminarun_id_seq" in

    let add_lane i sample_id =
      let lane_num = i + 1 in
      PGSQL(dbh) "INSERT INTO data_lane (illuminarun_id,number,sample_id)
                  VALUES ($illuminarun_id,$lane_num,$sample_id)"
    in
    List.iteri add_lane samples;

    PGOCaml.commit dbh;
    illuminarun_id

end
