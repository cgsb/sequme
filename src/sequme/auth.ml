(** Authorization features. *)

open Batteries_uni;; open Printf

module rec User : sig
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
    (** [of_id dbh id] returns the user with given [id], or
        None if no such user exists. *)

  val of_username : (string,bool) Hashtbl.t PGOCaml.t -> string -> t option
    (** [of_username dbh name] returns the user with given [name], or
        None if no such user exists. *)

  val of_username_exn : (string,bool) Hashtbl.t PGOCaml.t -> string -> t
    (** Like [of_username] but raise [Error] if user does not exist. *)

  val primary_group : (string,bool) Hashtbl.t PGOCaml.t -> t -> Group.t
    (** Every user [x] has a "primary group", which has the same name
        as the user and whose sole member is the user. [primary_group
        dbh user] returns [user]'s primary group. *)

  val groups : (string,bool) Hashtbl.t PGOCaml.t -> t -> Group.t list
    (** Get the given user's groups. *)
end = struct
  exception Error of string

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

  let of_username_exn dbh name =
    let xs = PGSQL(dbh)
      "SELECT id,username,first_name,last_name,
              email,password,is_staff,is_active,
              is_superuser,last_login,date_joined
       FROM auth_user WHERE username=$name"
    in match xs with
      | [] -> Error (sprintf "unknown user %s" name) |> raise
      | (id,username,first_name,last_name,
        email,password,is_staff,is_active,
        is_superuser,last_login,date_joined)::[] ->
          {id;username;first_name;last_name;
           email;password;is_staff;is_active;
           is_superuser;last_login;date_joined}
      | _ -> assert false

  let of_username dbh name =
    try Some (of_username_exn dbh name)
    with Error _ -> None

  let primary_group dbh user =
    let username = user.username in
    PGOCaml.begin_work dbh;
    let ans =
      PGSQL(dbh) "SELECT id FROM auth_group WHERE name=$username"
      |> List.hd |> Group.of_id dbh |> Option.get
    in
    PGOCaml.commit dbh;
    ans

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
