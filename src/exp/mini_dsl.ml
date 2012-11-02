
#use "topfind";;
#thread ;;
#camlp4o;;
#require "sequme";;
#require "sexplib.syntax";;

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

  type handle = (int * string) list ref

  let init () = return (ref [])


  let get handle k =
    match List.Assoc.find !handle k with
    | Some s -> return s
    | None -> error (`get_error "not found")

  let delete_atomic handle k =
    handle := List.filter !handle (fun (key, _) -> key <> k)

  let delete handle k =
    handle := List.filter !handle (fun (key, _) -> key <> k);
    return ()

  let store handle k v =
    delete_atomic handle k;
    handle := (k, v) :: !handle;
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
  | Array of content array
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
  | `Array of content_type
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
    mutable function_types:  (string * content_type list * content_type) list;
  }
  with sexp

  type runtime = {
    dsl: dsl;
    backend_handle: Backend.handle;
    mutable last_id: int;
  }

  let dsl () = { value_types = []; function_types = []}

  let new_runtime configuration =
    Backend.init configuration
    >>= fun backend_handle ->
    return { dsl = dsl (); backend_handle; last_id = 0 }
      
  let new_value_type ~runtime name content =
    runtime.dsl.value_types <- (name, content) :: runtime.dsl.value_types
  let new_function_type ~runtime name arguments result =
    runtime.dsl.function_types <- (name, arguments, result) ::
      runtime.dsl.function_types

  let new_id runtime =
    runtime.last_id <- runtime.last_id + 1;
    runtime.last_id
      
  let new_value ~runtime ~type_name content =
    Option.(
      List.Assoc.find runtime.dsl.value_types type_name
      >>= fun content_type ->
    (* check content_type Vs content *)
      return {r_type = type_name; r_created = Time.now ();
              r_last_modified = Time.now (); r_id = new_id runtime;
              r_value = content}
    )

end



type final_error = [
| `io_exn of exn
| `init_error of string
]
with sexp_of

let run  (m : unit -> (unit, final_error) t) =
  match Lwt_main.run (m ()) with
  | Ok () -> ()
  | Error e -> eprintf "ERROR:\n  %s\n"
    (Sexp.to_string_hum (sexp_of_final_error e))
  
let test () =
  Dsl.new_runtime ()
  >>= fun runtime ->
  eprintf "start test\n%!";
  Dsl.(new_value_type ~runtime "person" (`Record [ "name", `String; "email", `String ]));
  let p1 =
    Dsl.(new_value ~runtime ~type_name:"person"
           (Record ["name", String "The Name"; "email", String "the@name.org"]))
  in
  
  ignore p1;
  
  return ()

  
