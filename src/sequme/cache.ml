open Batteries_uni;; open Printf

exception Invalid of string

let download url out_file =
  let open Http_client in
  let pipeline = new pipeline in
  let get_call = new get url in
  get_call # set_response_body_storage (`File (fun () -> out_file));
  pipeline # add get_call;
  pipeline # run()

let get conf url =
  let sequme_root = Map.StringMap.find "sequme_root" conf in
  let temp_dir = Filename.concat sequme_root "tmp" in
  let temp_file = Filename.temp_file ~temp_dir "" "" in
  
  download url temp_file;
  
  let md5sum = Digest.file temp_file |> Digest.to_hex in
  
  (* move temp file to cache directory *)
  let dest_file = List.fold_left Filename.concat "" [sequme_root; "db"; "cache"; md5sum] in
  sprintf "mv %s %s" temp_file dest_file |> Sys.command |> ignore;
  
  (* add entry in database *)
  let stmt = sprintf "INSERT INTO cache (md5sum, url) values ('%s', '%s')" md5sum url in
  Conf.sqlite_exec conf stmt


let query_short_name conf short_name =
  let stmt = sprintf "SELECT md5sum FROM cache where short_name='%s'" short_name in
  let md5sums = ref [] in
  let cb row _ = match row with
    | [| Some md5sum |] -> md5sums := md5sum::!md5sums
    | _ -> assert false
  in
  Conf.sqlite_exec conf ~cb stmt;
  match !md5sums with
    | [] -> raise Not_found
    | md5sum::[] -> List.fold_left Filename.concat "" [Map.StringMap.find "sequme_root" conf; "db"; "cache"; md5sum]
    | _ -> Invalid (sprintf "multiple files in cache have short name %s" short_name) |> raise
