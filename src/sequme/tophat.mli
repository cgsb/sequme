(** Tophat support. See the {{:http://tophat.cbcb.umd.edu}Tophat
    website} for details. *)
open Batteries_uni

exception Error of string

(** Tophat command. *)
type cmd = private {
  exec : string;
  min_anchor_length : int option;
  solexa1_3_quals : bool;
  num_threads : int option;
  max_multihits : int option;
  no_coverage_search : bool;
  coverage_search : bool;
  butterfly_search : bool;
  index_base : string;
  reads1 : string list;
  reads2 : string list;
}

val make_cmd : ?exec:string -> ?min_anchor_length:int -> ?solexa1_3_quals:bool
  -> ?num_threads:int -> ?max_multihits:int -> ?no_coverage_search:bool
  -> ?coverage_search:bool -> ?butterfly_search:bool 
  -> string -> string list -> string list
  -> cmd
  (** Construct tophat command.

      Most arguments are optional. Omitting them means the option is
      not included when calling tophat. You can specify the name/path
      of the tophat executable with the [exec] argument, which by
      default is "tophat".

      The last three arguments are mandatory and correspond to the
      index base, reads1 files, and reads2 files. This follows the
      tophat command line format; see the
      {{:http://tophat.cbcb.umd.edu/manual.html}Tophat Manual} for
      details.

      Raise [Error] if given arguments do not specify a
      valid tophat command. *)

val cmd_to_string : cmd -> string
  (** [to_string cmd] returns the string that can be typed directly on the
      command line to run tophat. *)
