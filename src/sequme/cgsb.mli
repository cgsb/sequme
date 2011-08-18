(** CGSB Core Facility. *)
open Batteries_uni

module Sample : sig
  exception Error of string

  type t = {
    name1 : string;
    investigator : string option;
  }
end

module Library : sig
  exception Error of string

  type t = {
    sample : Sample.t;
    index : Illumina.Barcode.t;
    read_type : ReadType.t;
    read_length : int; (** For paired-end, this is length of each end. *)
  }
end

module LibraryDB : sig
  exception Error of string

  type t = Library.t Map.IntMap.t

  val of_file : string -> t

end
