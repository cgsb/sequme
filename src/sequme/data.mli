(** Raw data. *)
open Batteries_uni

module CachedFile : sig
  type t = {
    id : int32;
    owner_id : int32;
    added_by_id : int32;
    created : PGOCaml.timestamptz;
    last_modified : PGOCaml.timestamptz;
    name : string;
    url : string;
    md5sum : string;
    note : string;
  }
      
  val of_id : (string,bool) Hashtbl.t PGOCaml.t -> int32 -> t option
  val of_name : (string,bool) Hashtbl.t PGOCaml.t -> string -> t option
    
end
