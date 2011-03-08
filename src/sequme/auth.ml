(** Authorization features. *)

open Batteries_uni;; open Printf

module rec User : sig
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
    (** [of_id dbh id] returns the user with given [id], or
        None if no such user exists. *)

  val of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> t option
    (** [of_username dbh name] returns the user with given [name], or
        None if no such user exists. *)

  val group_id_of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> int32 option
    (** For every user [x], there is a group of the same name [x], whose
        sole member is [x]. [group_id_of_username dbh username] returns
        the id of this unique group for user [username], or None if no
        such user exists. *)

  val groups : (string,bool) Hashtbl.t PGOCaml.t -> t -> Group.t list
    (** Get the given user's groups. *)
end = struct
  type t = {
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

  let of_id dbh id =
    let xs = PGSQL(dbh)
      "SELECT id,username,first_name,last_name,
              email,password,is_staff,is_active,
              is_superuser,last_login,date_joined
       FROM auth_user WHERE id=$id"
    in match xs with
      | [] -> None
      | (id,username,first_name,last_name,
        email,password,is_staff,is_active,
        is_superuser,last_login,date_joined)::[] ->
          Some {id;username;first_name;last_name;
                email;password;is_staff;is_active;
                is_superuser;last_login;date_joined}
      | _ -> assert false

  let of_username dbh name =
    let xs = PGSQL(dbh)
      "SELECT id,username,first_name,last_name,
              email,password,is_staff,is_active,
              is_superuser,last_login,date_joined
       FROM auth_user WHERE username=$name"
    in match xs with
      | [] -> None
      | (id,username,first_name,last_name,
        email,password,is_staff,is_active,
        is_superuser,last_login,date_joined)::[] ->
          Some {id;username;first_name;last_name;
                email;password;is_staff;is_active;
                is_superuser;last_login;date_joined}
      | _ -> assert false

  (* assumes there is a group with the same name as username *)
  let group_id_of_username dbh username =
    match PGSQL(dbh) "SELECT id FROM auth_group WHERE name=$username" with
      | [] -> None
      | x::[] -> Some x
      | _ -> assert false

  let groups dbh t =
    let id = t.id in
    PGOCaml.begin_work dbh;
    let ans = PGSQL(dbh) "SELECT group_id
                          FROM auth_user_groups
                          WHERE user_id=$id"
    |> List.map (Group.of_id dbh |- Option.get)
    in
    PGOCaml.commit dbh;
    ans
end

and Group : sig
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

  val members : (string,bool) Hashtbl.t PGOCaml.t -> t -> User.t list
    (** Get the given group's members. *)
end = struct
  type t = {
    id : int32;
    name : string;
  }

  let of_id dbh id =
    let xs = PGSQL(dbh) "SELECT id,name FROM auth_group WHERE id=$id"
    in match xs with
      | [] -> None
      | (id,name)::[] -> Some {id;name}
      | _ -> assert false

  let of_name dbh name =
    let xs = PGSQL(dbh) "SELECT id,name FROM auth_group WHERE name=$name"
    in match xs with
      | [] -> None
      | (id,name)::[] -> Some {id;name}
      | _ -> assert false

  let members dbh t =
    let id = t.id in
    PGOCaml.begin_work dbh;
    let ans = PGSQL(dbh)
      "SELECT user_id FROM auth_user_groups WHERE group_id=$id"
      |> List.map (User.of_id dbh |- Option.get)
    in
    PGOCaml.commit dbh;
    ans

end
