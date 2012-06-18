module Tls: sig
  val init :  unit -> unit

  val tls_connect :
    Lwt_unix.file_descr ->
    Ssl.context ->
    (Lwt_ssl.socket, [> `io_exn of exn ]) Sequme_flow.t
  val tls_shutdown :
    Lwt_ssl.socket -> (unit, [> `io_exn of exn ]) Sequme_flow.t

end
module Server: sig

  val tls_context :
    ?ca_certificate:string ->
    (string * string) ->
    (Ssl.context, [> `tls_context_exn of exn ]) Sequme_flow.t


  val tls_accept_loop :
    ?check_client_certificate:(Ssl.certificate ->
                               ([< `expired of
                                   string * Core.Std.Time.t
                                | `not_found of string
                                | `revoked of
                                    string * Core.Std.Time.t
                                | `valid of string ],
                                [> `io_exn of exn
                                | `not_an_ssl_socket
                                | `tls_accept_error of exn ]
                                  as 'a)
                                 Sequme_flow.t) ->
    Ssl.context ->
    port:int ->
    (Lwt_ssl.socket ->
     [> `anonymous_client
     | `invalid_client of
         [> `expired of string * Core.Std.Time.t
         | `not_found of string
         | `revoked of string * Core.Std.Time.t
         | `wrong_certificate ]
     | `valid_client of string ] ->
     (unit, 'a) Sequme_flow.t) ->
    (unit, [> `io_exn of exn | `socket_creation_exn of exn ])
      Sequme_flow.t

end

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

