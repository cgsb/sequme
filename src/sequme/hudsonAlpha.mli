(** Download data from HudsonAlpha. *)

open Batteries_uni

type status_html = string
    (** Contents of http://hts.hudsonalpha.org/status/ as a
        string. *)
    
type headers = string list
type data_rows = string list list
type table = headers * data_rows

type libid

val get_status_html : string -> status_html
  (** [get_status_html passwd] returns the contents of
      http://hts.hudsonalpha.org/status/ as a string. Must provide
      password for user "LittmanLab". *)

val get_marias_table : status_html -> table
  (** Extract Maria's data sets. *)

val get_sequenced_libids : table -> libid list
  (** Return the [Lib Id]s of just those data sets whose status is
      "Sequenced" from given table. *)

val cmp_libid : libid -> libid -> int
  (** Compare two [Lib Id]s. E.g. [cmp_libid "SL340" "SL346"] =
      1. Note this is reverse lexicographic order. *)

val download_fastq : string -> libid -> unit
  (** [download_fastq dir libid] will download the fastq file for data
      set [libid] to [dir]. *)
