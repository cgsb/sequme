module Tls: sig
  val init : ?thread_safe:bool -> unit -> unit

  val tls_connect :
    Lwt_unix.file_descr ->
    Ssl.context ->
    (Lwt_ssl.socket, [> `io_exn of exn ]) Sequme_flow.t
  val tls_shutdown :
    Lwt_ssl.socket -> (unit, [> `io_exn of exn ]) Sequme_flow.t

end
module Server: sig

  val tls_context :
    ?with_client_authentication:[ `CA_certificate of string
                                | `CA_path of string ] ->
    (string * string) ->
    (Ssl.context, [> `tls_context_exn of exn ]) Sequme_flow.t


  val tls_accept_loop :
    ?check_client_certificate:(Ssl.certificate ->
                               ([< `certificate_not_found
                                | `expired
                                | `ok
                                | `revoked ],
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
         [> `certificate_not_found of Ssl.certificate
         | `expired_certificate of Ssl.certificate
         | `revoked_certificate of Ssl.certificate
         | `wrong_certificate ]
     | `valid_client of Ssl.certificate ] ->
     (unit, 'a) Sequme_flow.t) ->
    (unit, [> `io_exn of exn | `socket_creation_exn of exn ])
      Sequme_flow.t
end

module Client: sig

  val tls_context :
    ?verification_policy:[ `client_makes_sure | `ok_self_signed ] ->
    [ `anonymous | `with_pem_certificate of string ] ->
    (Ssl.context, [> `tls_context_exn of exn ]) Sequme_flow.t

end
