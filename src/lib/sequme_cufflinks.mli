(** Cufflinks support. See the
    {{:http://cufflinks.cbcb.umd.edu}Cufflinks website} for details. *)
open Batteries_uni

exception Error of string

(** Cufflinks command. *)
type cmd = private {
  exec : string;
  output_dir : string option;
  num_threads : int option;
  mask_file : string option;
  quartile_normalization : bool;
  gtf : string option;
  aligned_reads : string
}

val make_cmd : ?exec:string -> ?output_dir:string -> ?num_threads:int
  -> ?mask_file:string -> ?quartile_normalization:bool -> ?gtf:string
  -> string -> cmd
  (** Construct cufflinks command. See the
      {{:http://cufflinks.cbcb.umd.edu/manual.html}Cufflinks manual} for
      details. *)

val cmd_to_string : cmd -> string
