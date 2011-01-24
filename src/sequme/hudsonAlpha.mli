(** HudsonAlpha datasets. *)
open Batteries_uni;; open Biocaml

exception Error of string

(** {6 Download from hudsonalpha.org} *)

type status_html = string
    (** Contents of http://hts.hudsonalpha.org/status/ as a
        string. *)
    
type libid = string

val get_status_html : string -> status_html
  (** [get_status_html passwd] returns the contents of
      http://hts.hudsonalpha.org/status/ as a string. Must provide
      password for user "LittmanLab". *)

val get_marias_table : status_html -> Table.t
  (** Extract Maria's data sets. Raise [Error] if cannot extract
      expected table from given [status_html]. *)

val get_sequenced_libids : Table.t -> libid list
  (** Return the [Lib Id]s of just those data sets whose status is
      "Sequenced" from given table. *)

val cmp_libid : libid -> libid -> int
  (** Compare two [Lib Id]s. E.g. [cmp_libid "SL340" "SL346"] =
      1. Note this is reverse lexicographic order. *)

val download_fastq : string -> libid -> unit
  (** [download_fastq dir libid] will download the fastq file for data
      set [libid] to [dir]. *)


(** {6 Local Datasets} *)

val fastq_path_of_libid : Conf.t -> libid -> string
  (** Return absolute path to fastq file for given [libid]. Raise
      [Failure] if fastq file not available for [libid]. *)

val is_single_end : Conf.t -> libid -> bool
  (** Return true if [libid] is for single end data. *)
