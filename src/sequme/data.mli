(** Raw data. *)
open Batteries_uni

module CachedFile : sig
  exception Error of string

  type db_record = private {
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

  val db_record_of_name : (string,bool) Hashtbl.t PGOCaml.t -> string -> db_record option
  val db_record_of_name_exn : (string,bool) Hashtbl.t PGOCaml.t -> string -> db_record
end

module Sample : sig
  exception Error of string

  val insert : ?owner:string -> made_by:string
    -> ?created:PGOCaml.timestamptz
    -> name:string -> note:string -> exp_type:string
    -> ?ref_genome_name:string -> cell_line:string -> conditions:string
    -> factor:string -> antibody:string -> ?concentration:float
    -> mean_fragment_size:int32 -> ?barcodes:string list
    -> (string,bool) Hashtbl.t PGOCaml.t -> int32
    (** Insert a new sample into the database. Arguments are:
        - owner - owner of this record in the database, defaults to [made_by] if not provided
        - made_by - the experimenter who prepared the sample
        - created - time the sample was created, default is current time
        - name - the unique name of the sample
        - note - free text not describing the sample
        ...
        - barcodes - list of barcodes used to multiplex the sample if any, omitting the argument and passing the empty list are equivalent

        @raise Error if given arguments invalid for any reason. *)
end

module IlluminaRun : sig
  exception Error of string

  type status = InProgress | Successful | Failed

  val insert : ?owner:string -> ?run_by:string -> run_at:string
    -> ?started:PGOCaml.timestamptz -> ?finished:PGOCaml.timestamptz
    -> status:status -> ?note:string -> samples:int32 list
    -> (string,bool) Hashtbl.t PGOCaml.t -> int32
    (** Record a new run of an Illumina machine. Arguments are:
        - owner - owner of this record in the database, defaults to [run_by] if provided
        - run_by - the experimenter who conducted this run if known
        - run_at - short name of the institute where this run was conducted
        - started - time the run was started, default is current time
        - finished - time the run was completed, only if successful or failed
        - status - in progress, successful, or failed
        - note - free text note describing the run
        - samples - list of sample IDs on this run, must be in order by lane number

        @raise Error if given arguments invalid for any reason. *)
end
