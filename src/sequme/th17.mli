(** Th17 Project. *)
open Batteries_uni

exception Error of string

module Lane : sig
  val id_of_sl_id : (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t -> string -> int32
    (** [id_of_sl_id dbh x] returns the ID of the lane on which the
        sample with SL ID [x] was run.
        
        @raise Error if the sample was not run or run multiple times.
    *)

  val sample_id_of_id : (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t -> int32 -> int32
    (** [sample_id_of_id dbh id] returns the sample ID associated with
        the lane whose ID = [id].

        @raise Error if there is no lane with ID = [id].
    *)

  val fastq_file_path_of_id : string -> int32 -> string
    (** [fastq_file_path_of_id sequme_root id] returns the full path of the
        fastq file for lane with ID = [id].

        @raise Error if lane is unknown or fastq file does not exist.
    *)
end

module Bowtie : sig
  val run : Conf.t -> (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t -> string -> unit
    (** [run conf sl_id] runs bowtie on the sample with given SL ID
        using the parameters decided on for the Th17 project.
        
        @raise Error if there is not exactly 1 lane for given SL
        ID. *)

  val any_id_of_sl_id : (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t -> string -> int32
    (** [any_id_of_sl_id dbh sl_id] returns ID for a bowtie run on
        sample with SL ID = [sl_id]. An arbitrary run is chosen if there
        are multiple runs for the same sample.

        @raise Error if there is no bowtie run for the requested
        sample.
    *)

  val delete : Conf.t -> (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t -> int32 -> unit
    (** [delete conf dbh id] deletes the database record and
        associated files for bowtie run with ID = [id].

        @raise Error if no such run.
    *)

  val sam_file_path_of_id : string -> int32 -> string
    (** [sam_file_path_of_id sequme_root id] returns the full path of
        the SAM file for the bowtie run with ID = [id].

        @raise Error if bowtie run unknown or if SAM file does not
        exist.  *)
end
