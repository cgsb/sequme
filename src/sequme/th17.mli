(** Th17 Project. *)
open Batteries_uni

exception Error of string

type dbh = (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t
type timestamptz = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t

module Lane : sig
  val id_of_sl_id : dbh -> string -> int32
    (** [id_of_sl_id dbh x] returns the ID of the lane on which the
        sample with SL ID [x] was run.
        
        @raise Error if the sample was not run or run multiple times.
    *)

  val sample_id_of_id : dbh -> int32 -> int32
    (** [sample_id_of_id dbh id] returns the sample ID associated with
        the lane whose ID = [id].

        @raise Error if there is no lane with ID = [id].
    *)

  val fastq_file_path_of_id : string -> int32 -> string
    (** [fastq_file_path_of_id sequme_root id] returns the full path of the
        fastq file for lane with ID = [id].

        @raise Error if lane is unknown or fastq file does not exist.
    *)

  val not_downloaded_list : string -> dbh -> string list
    (** [not_downloaded_list password] returns the list of sequenced
        data sets available at HudsonAlpha but not yet downloaded to
        sequme database. *)

end

module Bowtie : sig
  val run : Conf.t -> dbh -> string -> unit
    (** [run conf sl_id] runs bowtie on the sample with given SL ID
        using the parameters decided on for the Th17 project.
        
        @raise Error if there is not exactly 1 lane for given SL
        ID. *)

  val any_id_of_sl_id : dbh -> string -> int32
    (** [any_id_of_sl_id dbh sl_id] returns ID for a bowtie run on
        sample with SL ID = [sl_id]. An arbitrary run is chosen if there
        are multiple runs for the same sample.

        @raise Error if there is no bowtie run for the requested
        sample.
    *)

  val delete : Conf.t -> dbh -> int32 -> unit
    (** [delete conf dbh id] deletes the database record and
        associated files for bowtie run with ID = [id].

        @raise Error if no such run.
    *)

  val sam_file_path_of_id : string -> int32 -> string
    (** [sam_file_path_of_id sequme_root id] returns the full path of
        the SAM file for the bowtie run with ID = [id].

        @raise Error if bowtie run unknown or if SAM file does not
        exist.  *)

  val was_success : string -> int32 -> bool
    (** [was_success sequme_root id] returns true if the Bowtie messages
        to stderr indicate the run completed successfully. Note this
        is independent of the status value in the database, although
        presumably the database will be updated based on this
        value. *)

  val post_process : string -> dbh -> int32 -> unit
    (** [post_process sequme_root dbh] does any post processing
        necessary after a call to {!run}. *)

end

module Macs : sig
  val run : Conf.t -> dbh -> string -> string -> unit
    (** [run conf dbh treatment control] runs MACS on the given
        treatment and control samples, specified by their SL IDs. If
        multiple bowtie runs are available for the given sample or
        control, an arbitrary one is chosen. *)

  val delete : Conf.t -> dbh -> int32 -> unit
    (** [delete conf dbh id] deletes the database record and
        associated files for MACS run with ID = [id].

        @raise Error if no such run.
    *)

  val last_info_datetime : string -> int32 -> timestamptz option
    (** [last_info_datetime sequme_root id] returns the timestamp of
        the last INFO log that MACS printed to stderr. *)

  val was_success : string -> int32 -> bool
    (** [was_success sequme_root id] returns true if the MACS messages
        to stderr indicate the run completed successfully. Note this
        is independent of the status value in the database, although
        presumably the database will be updated based on this
        value. *)

  val post_process : string -> dbh -> int32 -> unit
    (** [post_process sequme_root dbh] does any post processing
        necessary after a call to {!run}. *)

end
