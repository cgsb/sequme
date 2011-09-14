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

val path_of_index : Conf.t -> string -> string
  (** [path_of_index i] returns path to the directory for index [i],
      including the basename of the index files upto the first
      dot. Raise [Not_found] if index unavailable. *)
