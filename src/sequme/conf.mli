(** Sequme configuration. *)
open Batteries_uni

exception Invalid of string

type t = string Map.StringMap.t

val read : string -> t
  (** [read sequme_root] reads the configuration file located within
      the given [sequme_root] directory. Raise [Invalid] if any
      errors. *)

type dbconf = {
  db_engine : string;
  db_name : string;
  db_user : string;
  db_password : string;
  db_host : string;
  db_port : string
}

val get_dbconf : t -> dbconf
  (** Return dbconf in [t]. Raise [Invalid] if [t] does not fully
      specify a database configuration. *)
