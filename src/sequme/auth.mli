(** Authorization features. *)
open Batteries_uni

module User : sig
  exception Error of string

  type t = private {
    id : int32;
    username : string;
    first_name : string;
    last_name : string;
    email : string;
    password : string;
    is_staff : bool;
    is_active : bool;
    is_superuser : bool;
    last_login : PGOCaml.timestamptz;
    date_joined : PGOCaml.timestamptz;
  }

  val of_id : (string,bool) Hashtbl.t PGOCaml.t -> int32 -> t option
    (** [of_username dbh id] returns the user with given [id], or
        None if no such user exists. *)

  val of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> t option
    (** [of_username dbh name] returns the user with given [name], or
        None if no such user exists. *)

  val group_id_of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> int32 option
    (** For every user [x], there is a group of the same name [x], whose
        sole member is [x]. [group_id_of_username dbh username] returns
        the id of this unique group for user [username], or None if no
        such user exists. *)
end

module Group : sig
  type t = private {
    id : int32;
    name : string;
  }

  val of_id : (string,bool) Hashtbl.t PGOCaml.t -> int32 -> t option
    (** [of_id dbh id] returns the group with given [id], or
        None if no such group exists. *)

  val of_name : (string,bool) Hashtbl.t PGOCaml.t -> string -> t option
    (** [of_name dbh name] returns the group with given [name], or
        None if no such group exists. *)
end
