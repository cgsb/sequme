(*

  ocamlfind ocamlc  -package sequme -thread -package sexplib.syntax -linkpkg -syntax camlp4o src/exp/mini_dsl.ml -o mini && mini

*)

open Core.Std
  
module Flow = struct
  include Sequme_flow
  include Sequme_flow_list
end

open Flow

type configuration = unit
  
module type BACKEND = sig

  type handle

  val init: configuration -> (handle, [> `init_error of string ]) Flow.t

  val store: handle -> int -> string -> (unit, [> `store_error of string ]) Flow.t
    
  val get: handle -> int -> (string, [> `get_error of string]) Flow.t

  val delete: handle -> int -> (unit, [> `delete_error of string]) Flow.t
end

module In_memory : BACKEND = struct

  type handle = {
    mutable db: (int * string) list;
    mutable last_id: int;
  }

  let init () = return {db = []; last_id = 0;}


  let get handle k =
    match List.Assoc.find handle.db k with
    | Some s -> return s
    | None -> error (`get_error "not found")

  let delete_atomic handle k =
    handle.db <- List.filter handle.db (fun (key, _) -> key <> k)

  let delete handle k =
    handle.db <- List.filter handle.db (fun (key, _) -> key <> k);
    return ()

  let fresh_id handle =
    handle.last_id <- handle.last_id + 1;
    handle.last_id
    
  let store handle k v =
    delete_atomic handle k;
    handle.db <- (k, v) :: handle.db;
    return ()

end

module Backend = In_memory
  
module Dsl = struct

  type volume_content =
  | Link of int
  | Directory of (string * string list)
  with sexp
    
  type volume = {
    v_id: int;
    v_last_modified: Time.t;
    v_content: volume_content;
    v_deleted: int option;
  }
  with sexp
    
  type content =
  | Int of int
  | Float of float
  | String of string
  | Timestamp of Time.t
  | List of content list
  | Pointer of int
  | Record of (string * content) list
  | Volume of int
  with sexp

  type content_type = [
  | `Int
  | `Float
  | `String
  | `Timestamp
  | `List of content_type
  | `Pointer
  | `Record of (string * content_type) list
  | `Volume
  ]
  with sexp
    
  type value = {
    r_type: string;
    r_created: Time.t;
    r_last_modified: Time.t;
    r_id: int;
    r_value: content;
  }
  with sexp

  type evaluation = {
    f_type: string;
    f_inserted: Time.t;
    f_started: Time.t;
    f_completed: Time.t;
    f_input: content_type list;
    f_status: [ `inserted | `started | `failed of string | `succeeded of content_type ];
  }
  with sexp

  type dsl = {
    mutable value_types: (string * content_type) list;
    mutable function_types:  (string * (content_type list * content_type)) list;
  }
  with sexp

  type runtime = {
    dsl: dsl;
    backend_handle: Backend.handle;
  }

  let dsl () = { value_types = []; function_types = []}

  let new_runtime configuration =
    Backend.init configuration
    >>= fun backend_handle ->
    return { dsl = dsl (); backend_handle; }
      
  let new_value_type ~runtime name content =
    match List.Assoc.find runtime.dsl.value_types name with
    | Some s -> error (`type_name_already_used (name, s))
    | None ->
      runtime.dsl.value_types <- (name, content) :: runtime.dsl.value_types;
      return ()
        
  let new_function_type ~runtime name arguments result =
    match List.Assoc.find runtime.dsl.function_types name with
    | Some s -> error (`type_name_already_used (name, s))
    | None ->
      runtime.dsl.function_types <-
        (name, (arguments, result)) :: runtime.dsl.function_types;
      return ()

      
  let rec type_check ~runtime content_type content =
    match content_type, content with
    | Int _, `Int -> return ()
    | Float _, `Float -> return ()
    | String _, `String -> return ()
    | Timestamp _, `Timestamp -> return ()
    | List l, `List t ->
      while_sequential l (fun c -> type_check ~runtime c t)
      >>= fun _ ->
      return ()
    | Pointer _, `Pointer -> return ()
    | Record c, `Record t ->
      begin match List.zip c t with
      | Some z ->
        while_sequential z
          (fun ((cn, ct), (tn, tt)) ->
            if cn = tn then
              type_check ~runtime ct tt
            else
              error (`wrong_typing (`record_names_mismatch (cn, tn))))
        >>= fun _ ->
        return ()
      | None -> error (`wrong_typing (`record_fields_mismatch (c, t)))
      end
    | Volume _, `Volume -> return ()
    | c, t -> error (`wrong_typing (`type_mismatch (c, t)))

  let get_value_type ~runtime type_name =
    match List.Assoc.find runtime.dsl.value_types type_name with
    | Some t -> return t
    | None -> error (`unbound_type type_name)
    
  let new_value ~runtime ~type_name content =
    get_value_type ~runtime type_name
    >>= fun content_type ->
    type_check ~runtime content content_type
    >>= fun () ->
    return {r_type = type_name; r_created = Time.now ();
            r_last_modified = Time.now (); r_id = 0 (* TODO *);
            r_value = content}


end



type final_error = 
[ `init_error of string
| `type_name_already_used of string * Dsl.content_type
| `unbound_type of string
| `wrong_typing of
    [ `record_fields_mismatch of
        (string * Dsl.content) list *
          (string * Dsl.content_type) Core.Std.List.t
    | `record_names_mismatch of string * string
    | `type_mismatch of Dsl.content * Dsl.content_type ] ]
with sexp_of

let p fmt =
  ksprintf (fun s ->
    eprintf "TEST: %s\n%!"
      (String.split ~on:'\n' s
       |! List.map ~f:(sprintf "%s")
       |! String.concat ~sep:"\n    ")) fmt
    
let run  (m : unit -> (unit, final_error) t) =
  match Lwt_main.run (m ()) with
  | Ok () -> ()
  | Error e -> eprintf "ERROR:\n  %s\n"
    (Sexp.to_string_hum (sexp_of_final_error e))
  
let make_person_dsl () =
  p "make_person_dsl";
  Dsl.new_runtime ()
  >>= fun runtime ->
  Dsl.new_value_type ~runtime "person" (`Record [ "name", `String; "email", `String ])
  >>= fun () ->
  return runtime

let test_ok () =
  p "test_ok";
  make_person_dsl () >>= fun runtime ->
  Dsl.(new_value ~runtime ~type_name:"person"
         (Record ["name", String "The Name"; "email", String "the@name.org"]))
  >>= fun _ ->
  return ()

let test_ko1 () =
  p "test_ko1";
  make_person_dsl () >>= fun runtime ->
  Dsl.(new_value ~runtime ~type_name:"person"
         (Record ["wrong_name", String "The Name"; "email", String "the@name.org"]))
  >>= fun _ ->
  return ()

let test_ko2 () =
  p "test_ko2";
  make_person_dsl () >>= fun runtime ->
  Dsl.(new_value ~runtime ~type_name:"person"
         (Record ["name", Int 42; "email", String "the@name.org"]))
  >>= fun _ ->
  return ()
let () =
  run test_ok;
  run test_ko1;
  run test_ko2;
  ()

  
