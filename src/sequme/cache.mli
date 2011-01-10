(** Download and cache web downloads. Can also add files manually, in
    which case a simple file name, instead of a full URL, identifies the
    file. *)

open Batteries_uni

exception Invalid of string

val get : Conf.t -> string -> unit
  (** [get conf url] downloads the file at [url] and adds it to the
      cache dictated by [conf]. TO DO: inspect http header, and do not
      re-download if unnecessary. *)

val query_short_name : Conf.t -> string -> string
  (** [query_short_name conf short_name] searches the cache specified by
      [conf] for a file with given [short_name]. Returns absolute path
      to the cached file. Raise [Not_found] if no such file, or
      [Invalid] in case of other errors. *)
