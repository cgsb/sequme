open Batteries_uni;; open Printf

module CachedFile = struct
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

  let of_id dbh id =
    match PGSQL(dbh)
      "SELECT id,owner_id,added_by_id,created,
              last_modified,name,url,md5sum,note
       FROM data_cachedfile WHERE id=$id"
    with
      | [] -> None
      | (id,owner_id,added_by_id,created,
        last_modified,name,url,md5sum,note)::[] ->
          Some {id;owner_id;added_by_id;created;
                last_modified;name;url;md5sum;note}
      | _ -> assert false

  let of_name dbh name =
    match PGSQL(dbh)
      "SELECT id,owner_id,added_by_id,created,
              last_modified,name,url,md5sum,note
       FROM data_cachedfile WHERE name=$name"
    with
      | [] -> None
      | (id,owner_id,added_by_id,created,
        last_modified,name,url,md5sum,note)::[] ->
          Some {id;owner_id;added_by_id;created;
                last_modified;name;url;md5sum;note}
      | _ -> assert false

end
