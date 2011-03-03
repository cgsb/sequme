(** Users. *)

exception Error of string

val id_of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> int32 option
  (** [id_of_username name] returns the user id of the user named
      [name], or None if no such user exists. *)

val group_id_of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> int32 option
  (** For every user [x], there is a group of the same name [x], whose
      sole member is [x]. [group_id_of_username dbh username] returns
      the id of this unique group for user [username], or None if no
      such user exists. *)
