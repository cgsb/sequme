open Batteries_uni;; open Printf

module User = struct
  exception Error of string
    
  let id_of_username dbh name =
    match PGSQL(dbh) "SELECT id FROM auth_user where username=$name" with
      | [] -> None
      | x::[] -> Some x
      | _ -> assert false
          
  (* assumes there is a group with the same name as username *)
  let group_id_of_username dbh username =
    match PGSQL(dbh) "SELECT id FROM auth_group where name=$username" with
      | [] -> None
      | x::[] -> Some x
      | _ -> assert false
end
