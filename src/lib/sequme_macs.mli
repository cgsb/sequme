(** MACS support. See the
    {{:http://liulab.dfci.harvard.edu/MACS/}MACS website} for details. *)
open Sequme_internal_pervasives

exception Error of string

(** MACS command. *)
type cmd = private {
  exec : string;
  name : string option;
  format : string option;
  pvalue : string option;
  mfold : (int32 * int32) option;
  tsize : int32 option;
  gsize : string option;
  bw : int32 option;
  wig : bool;
  space : int32 option;
  control : string;
  treatment : string;
}

val make_cmd : ?exec:string
  -> ?name:string -> ?format:string
  -> ?pvalue:string -> ?mfold:(int32 * int32)
  -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
  -> ?wig:bool -> ?space:int32
  -> control:string -> treatment:string -> unit
  -> cmd

val cmd_to_string : cmd -> string
  (** [to_string cmd] returns the string that can be typed directly on the
      command line to run MACS. *)
