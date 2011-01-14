(** Cuffdiff support. See the
    {{:http://cufflinks.cbcb.umd.edu}Cuffdiff website} for details. *)
open Batteries_uni

exception Error of string

(** Cuffdiff command. *)
type cmd = private {
  exec : string;
  output_dir : string option;
  num_threads : int option;
  mask_file : string option;
  quartile_normalization : bool;
  gtf : string;
  samples : string list list
}

val make_cmd : ?exec:string -> ?output_dir:string -> ?num_threads:int
  -> ?mask_file:string -> ?quartile_normalization:bool
  -> string -> string list list -> cmd
  (** Construct cuffdiff command. See the
      {{:http://cufflinks.cbcb.umd.edu/manual.html#cuffdiff}Cuffdiff
      manual} for details. *)

val cmd_to_string : cmd -> string
