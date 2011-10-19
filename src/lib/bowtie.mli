(** Bowtie support. See the
    {{:http://bowtie-bio.sourceforge.net}Bowtie website} for details. *)
open Batteries_uni

exception Error of string

(** Bowtie command. *)
type cmd = private {
  exec : string;
  ebwt : string;
  phred33_quals : bool;
  phred64_quals : bool;
  k : int option;
  best : bool;
  sam : bool;
  threads : int option;
  reads : string;
  hit : string option;
}

val make_cmd : ?exec:string -> ebwt:string
  -> ?phred33_quals:bool -> ?phred64_quals:bool
  -> ?k:int
  -> ?best:bool -> ?sam:bool -> ?threads:int
  -> ?hit:string -> reads:string
  -> cmd

val cmd_to_string : cmd -> string
  (** [to_string cmd] returns the string that can be typed directly on the
      command line to run bowtie. *)
