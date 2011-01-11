(** Bowtie support. See the
    {{:http://bowtie-bio.sourceforge.net}Bowtie website} for details. *)
open Batteries_uni

val path_of_index : Conf.t -> string -> string
  (** [path_of_index i] returns path to the directory for index [i],
      including the basename of the index files upto the first
      dot. Raise [Not_found] if index unavailable. *)
