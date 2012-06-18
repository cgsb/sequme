val init_tls :  unit -> unit

class type ['a] connection =
object
  method in_channel: Lwt_io.input_channel 
  method out_channel: Lwt_io.output_channel 
  method shutdown : (unit, [> `io_exn of exn ] as 'a) Sequme_flow.t
end

type connection_specification = [
| `tls of
    [ `anonymous | `with_certificate of string * string ]
  * [ `verify_server | `allow_self_signed ]
| `plain
]
    

val connect: address:Lwt_unix.sockaddr -> connection_specification ->
  ([> `io_exn of exn] connection, [> `io_exn of exn | `tls_context_exn of exn ])
    Sequme_flow.t

val plain_server :
  port:int ->
  ([> `io_exn of exn ] connection ->
   (unit,
    [> `io_exn of exn
    | `not_an_ssl_socket
    | `tls_accept_error of exn ]) Sequme_flow.t) ->
  (unit, [> `io_exn of exn | `socket_creation_exn of exn ]) Sequme_flow.t

val tls_server :
  port:int ->
  cert_key:string * string ->
  ([> `io_exn of exn ] connection ->
   (unit,
    [> `io_exn of exn
    | `not_an_ssl_socket
    | `tls_accept_error of exn ])
     Sequme_flow.t) ->
  (unit,
   [> `io_exn of exn
   | `socket_creation_exn of exn
   | `tls_context_exn of exn ])
    Sequme_flow.t

type client_check_result =
[ `expired of string * Core.Std.Time.t
| `not_found of string
| `revoked of string * Core.Std.Time.t
| `valid of string ]

type client_kind = 
[ `anonymous_client
| `invalid_client of
    [ `expired of string * Core.Std.Time.t
    | `not_found of string
    | `revoked of string * Core.Std.Time.t
    | `wrong_certificate ]
| `valid_client of string ]

val authenticating_tls_server :
  ca_certificate:string ->
  check_client_certificate:(Ssl.certificate ->
                            (client_check_result,
                             [> `io_exn of exn
                             | `not_an_ssl_socket
                             | `tls_accept_error of exn ]
                               as 'a)
                              Sequme_flow.t) ->
  port:int ->
  cert_key:string * string ->
  ([> `io_exn of exn ] connection -> client_kind -> (unit, 'a) Sequme_flow.t) ->
  (unit,
   [> `io_exn of exn
   | `socket_creation_exn of exn
   | `tls_context_exn of exn ]) Sequme_flow.t

val authenticating_tls_server_with_ca :
  ca:Sequme_flow_certificate_authority.t ->
  port:int ->
  cert_key:string * string ->
  ([> `io_exn of exn ] connection -> client_kind ->
   (unit,
    [> `io_exn of exn
    | `not_an_ssl_socket
    | `tls_accept_error of exn
    | `wrong_subject_format of string ]) Sequme_flow.t) ->
  (unit,
   [> `io_exn of exn
   | `socket_creation_exn of exn
   | `tls_context_exn of exn ]) Sequme_flow.t
