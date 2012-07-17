type db_handle
type error = [
| `exn of exn
| `connection of exn
| `disconnection of exn
| `query of (string * exn)
]
type result_item = string option list
type result = result_item list
type query = string

val connect :
  ?host:string ->
  ?port:int ->
  ?database:string ->
  ?user:string ->
  ?password:string ->
  ?log:(string -> unit) ->
  unit ->
  (db_handle,
  [> `db_backend_error of [> error ] ]) Sequme_flow.t

val disconnect :
  dbh:db_handle ->
  (unit, [> `db_backend_error of [> `disconnection of exn ] ])
    Sequme_flow.t
val reconnect :
  dbh:db_handle ->
  (db_handle,
  [> `db_backend_error of
    [> `connection of exn | `disconnection of exn ] ])
    Sequme_flow.t

val query :
  dbh:db_handle ->
  query ->
  (result,
  [> `db_backend_error of [> `query of query * exn ] ]) Sequme_flow.t
