(** Sequme configuration. *)
open Batteries_uni

exception Invalid of string

type t = string Map.StringMap.t
    
val read : string -> t
  (** [read sequme_root] reads the configuration file located within
      the given [sequme_root] directory. Raise [Invalid] if any
      errors. *)
