open Batteries_uni;; open Printf

type dbh = (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t
type timestamptz = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t

module Sample = struct
  type barcode = {
    barcode_name : string;
    sequence : string;
  }

  type files = {
    fastq_file : string;
    qual_file : string;
    stats_file : string;
  }

  type typ =
      | NotBarcoded of files
      | Barcoded of (barcode * files) list

  type t = {
    id : int32 option;
    started : timestamptz option;
    finished : timestamptz option;
    made_by_id : int32;
    name : string;
    sample_name : string;
    read_type : string;
    note : string;
    typ : typ;
  }

  let add_new_barcode dbh sample_id (barcode,files) =
    let name = barcode.barcode_name in
    let sequence = barcode.sequence in
    let fastq_file = files.fastq_file in
    let qual_file = files.qual_file in
    let stats_file = files.stats_file in
    PGSQL(dbh)
      "INSERT INTO cgsb_barcode
       (name,sequence,sample_id,fastq_file,qual_file,stats_file)
       VALUES
       ($name,$sequence,$sample_id,$fastq_file,$qual_file,$stats_file)"
    ;
    PGOCaml.serial4 dbh "cgsb_barcode_id_seq"

  let add_new dbh t =
    let created = Util.now() in
    let last_modified = created in
    let made_by_id = t.made_by_id in
    let name = t.name in
    let sample_name = t.sample_name in
    let read_type = t.read_type in
    let note = t.note in
    let fastq_file,qual_file,stats_file = match t.typ with
      | NotBarcoded x -> x.fastq_file, x.qual_file, x.stats_file
      | Barcoded _ -> "", "", ""
    in
    PGSQL(dbh)
      "INSERT INTO cgsb_sample
       (created,last_modified,made_by_id,name,sample_name,read_type,note,fastq_file,qual_file,stats_file)
       VALUES
       ($created,$last_modified,$made_by_id,$name,$sample_name,$read_type,$note,$fastq_file,$qual_file,$stats_file)"
    ;
    let sample_id = PGOCaml.serial4 dbh "cgsb_sample_id_seq" in
    (match t.typ with
      | NotBarcoded _ -> ()
      | Barcoded bcl -> List.iter (add_new_barcode dbh sample_id |- ignore) bcl
    );
    sample_id

end

module Quad = struct
  type t = {
    id : int32 option;
    number : int;
    sample : Sample.t
  }

  let add_new dbh solidrun_id t =
    let number = t.number in
    let sample_id = Sample.add_new dbh t.sample in
    PGSQL(dbh)
      "INSERT INTO cgsb_quad
       (number,sample_id,solidrun_id)
       VALUES
       ($number,$sample_id,$solidrun_id)"
    ;
    PGOCaml.serial4 dbh "cgsb_quad_id_seq"

end

module SolidRun = struct
  type t = {
    id : int32 option;
    started : timestamptz option;
    finished : timestamptz option;
    run_by_id : int32;
    note : string;
    quads : Quad.t list
  }

  let add_new dbh t =
    let created = Util.now() in
    let last_modified = created in
    let run_by_id = t.run_by_id in
    let note = t.note in
    PGSQL(dbh)
      "INSERT INTO cgsb_solidrun
       (created,last_modified,run_by_id,note)
       VALUES
       ($created,$last_modified,$run_by_id,$note)"
    ;
    let solidrun_id = PGOCaml.serial4 dbh "cgsb_solidrun_id_seq" in
    List.iter (Quad.add_new dbh solidrun_id |- ignore) t.quads;
    solidrun_id

  (** All given files should be for same run and not have any barcoded
      samples. *)
  let make_run files =
    let make_quad file =
      let ledger = Ledger.of_file file in
      let find x = try Map.StringMap.find x ledger with Not_found -> "Not_found" in
      let open Sample in
      {
        Quad.id = None;
        Quad.number = (try find "quad" |> Int.of_string with Not_found | Failure _ -> 0);
        Quad.sample = {
          id = None;
          started = None;
          finished = None;
          made_by_id = find "lab" |> Ledger.lab_to_user_id;
          name = find "name";
          sample_name = find "sample_name";
          read_type = find "readtype";
          note = "";
          typ = NotBarcoded {
            fastq_file = find "csfasta_name";
            qual_file = find "qual_name";
            stats_file = "";
          }
        }
      }
    in
    {
      id = None;
      started = None;
      finished = None;
      run_by_id = 19l; (* Paul Scheid *)
      note = "";
      quads = List.map make_quad files;
    }

end
