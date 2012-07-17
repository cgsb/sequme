open Core.Std
open Sequme_flow

module Lwt_config = struct
  include Lwt
  include Lwt_chan
end
module PG = PGOCaml_generic.Make(Lwt_config)

type db_handle = {
  mutable connection: int PG.t; (* ensure it does not come from the
                                     syntax extension *)
  host: string option;
  port: int option;
  database: string option;
  user: string option;
  password: string option;
  log: (string -> unit) option
}
type error = [
| `exn of exn
| `connection of exn
| `disconnection of exn
| `query of (string * exn)
]
type result_item = string option list
type result = result_item list
type query = string
    
let connect ?host ?port ?database ?user ?password ?log () :
    (db_handle, [> `db_backend_error of [> error ] ]) Sequme_flow.t =
  bind_on_error
    (catch_io (PG.connect
                  ?host ?port ?database ?user ?password) ())
    (fun e -> error (`db_backend_error (`connection e)))
  >>= fun connection ->
  return {log; connection; host; port; database; user; password;}

let disconnect ~(dbh: db_handle) = 
  bind_on_error (catch_io PG.close dbh.connection)
    (fun e -> error (`db_backend_error (`disconnection e)))

let reconnect ~dbh =
  disconnect dbh >>= fun () ->
  let { host; port; database; user; password; _ } = dbh in
  bind_on_error
    (catch_io (PG.connect
                  ?host ?port ?database ?user ?password) ())
    (fun e -> error (`db_backend_error (`connection e)))
  >>= fun connection ->
  dbh.connection <- connection;
  return dbh
    
let logf dbh fmt =
  ksprintf (fun s ->
    match dbh.log with
      | Some f -> f s
      | None -> ()) ("DB:" ^^ fmt)

let query ~(dbh: db_handle) (query:query) =
  let name = sprintf "%f" (Random.float 100.) in
  let work_m = 
    wrap_io (PG.prepare ~name ~query dbh.connection) ()
    >>= fun () ->
    wrap_io (PG.execute ~name ~params:[] dbh.connection) ()
    >>= fun result ->
    wrap_io (PG.close_statement dbh.connection ~name) ()
    >>= fun () ->
    return (result: result)
  in
  double_bind work_m
    ~ok:(fun r ->
      logf dbh "QUERY: %S : SUCCESS" query;
      return r)
    ~error:(function
      | `io_exn e ->
          logf dbh "QUERY: %S : ERROR: %s" query (Exn.to_string e);
          error (`db_backend_error (`query (query, e))))
