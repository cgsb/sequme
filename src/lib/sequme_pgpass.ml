open Sequme_std

exception Error of string

type 'a star = Val of 'a | Star

type record = {
  hostname : string star;
  port : int star;
  database : string star;
  username : string star;
  password : string;
}

type t = record list

(** Split a colon separated string, allowing ':' and '\' to be
    escaped. *)
let split (s : string) : string list =
  let unescaped_slash_msg = "character '\\' must be escaped with '\\'" in
  let f (escaped, curr_str, finished_strings) c =
    match c with
      | ':' ->
          if escaped then
            false, c::curr_str, finished_strings
          else
            false, [], (curr_str |> List.rev |> String.implode)::finished_strings
      | '\\' ->
          if escaped then
            false, c::curr_str, finished_strings
          else
            true, curr_str, finished_strings
      | _ ->
          if escaped then
            raise (Error unescaped_slash_msg)
          else
            false, c::curr_str, finished_strings
  in
  let escaped, curr_str, finished_strings = String.fold_left f (false,[],[]) s in
  if escaped then
    raise (Error unescaped_slash_msg)
  else
    (curr_str |> List.rev |> String.implode)::finished_strings
    |> List.rev


let parse_string_star s =
  if s = "*" then Star else Val s


let parse_int_star s =
  if s = "*" then Star
  else 
    try Val (int_of_string s)
    with Failure _ -> raise (Error (sprintf "%s is not an integer" s))


let of_line file line_num s =
  try
    match split s with
      | [hostname;port;database;username;password] ->
          {
            hostname = parse_string_star hostname;
            port = parse_int_star port;
            database = parse_string_star database;
            username = parse_string_star username;
            password
          }
      | l ->
          raise (Error (sprintf "expected exactly 5 fields but found %d" (List.length l)))
  with
      Error msg -> raise (Error (sprintf "%s:%d: %s" file line_num msg))


let of_file_exn file =
  file |> File.lines_of
  |> Enum.mapi ~f:(fun i -> of_line file (i+1))
  |> List.of_enum

let (=* ) (x:'a) (y : 'a star) = match y with
  | Star -> true
  | Val y -> x = y

let is_match h p d u {hostname;port;database;username} =
  (h =* hostname) && (p =* port) && (d =* database) && (u =* username)

let find ~hostname ~port ~database ~username t =
  let rec loop = function
    | [] -> None
    | record::t ->
        if is_match hostname port database username record
        then Some record
        else loop t
  in
  loop t
