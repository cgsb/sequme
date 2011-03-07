open Batteries_uni;; open Printf

module User = struct
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
    let xs = PGSQL(dbh) "SELECT id,username,first_name,last_name,email,password,is_staff,is_active,is_superuser,last_login,date_joined
                           FROM auth_user where id=$id"
    in match xs with
      | [] -> None
      | (id,username,first_name,last_name,email,password,is_staff,is_active,is_superuser,last_login,date_joined)::[] ->
          Some {id;username;first_name;last_name;email;password;is_staff;is_active;is_superuser;last_login;date_joined}
      | _ -> assert false

  let of_username dbh name =
    let xs = PGSQL(dbh) "SELECT id,username,first_name,last_name,email,password,is_staff,is_active,is_superuser,last_login,date_joined
                           FROM auth_user where username=$name"
    in match xs with
      | [] -> None
      | (id,username,first_name,last_name,email,password,is_staff,is_active,is_superuser,last_login,date_joined)::[] ->
          Some {id;username;first_name;last_name;email;password;is_staff;is_active;is_superuser;last_login;date_joined}
      | _ -> assert false

  (* assumes there is a group with the same name as username *)
  let group_id_of_username dbh username =
    match PGSQL(dbh) "SELECT id FROM auth_group where name=$username" with
      | [] -> None
      | x::[] -> Some x
      | _ -> assert false
end

module Group = struct
  type t = {
    id : int32;
    name : string;
  }

  let of_id dbh id =
    let xs = PGSQL(dbh) "SELECT id,name FROM auth_group where id=$id"
    in match xs with
      | [] -> None
      | (id,name)::[] -> Some {id;name}
      | _ -> assert false

  let of_name dbh name =
    let xs = PGSQL(dbh) "SELECT id,name FROM auth_group where name=$name"
    in match xs with
      | [] -> None
      | (id,name)::[] -> Some {id;name}
      | _ -> assert false

end
