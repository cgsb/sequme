open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

let dbg fmt =
  ksprintf (fun s ->
    let indented =
      s |! String.split ~on:'\n' |! String.concat ~sep:"\n          " in
    wrap_io (Lwt_io.eprintf "𝕱☏-DEBUG: %s\n%!") indented) fmt
  
module Tls = struct

  let init = Ssl.init ~thread_safe:true
    
  let accept socket context =
    bind_on_error
      (catch_io (Lwt_ssl.ssl_accept socket) context)
      (fun e -> error (`tls_accept_error e))

  let server_socket ~port =
    let open Lwt_unix in
    try
    let fd = socket PF_INET SOCK_STREAM 6 in
    bind fd (ADDR_INET (Unix.Inet_addr.bind_any, port));
    listen fd port;
    return fd
    with
    | e -> error (`socket_creation_exn e)

  let tls_connect socket ssl_context =
    wrap_io (Lwt_ssl.ssl_connect socket) ssl_context

  let tls_shutdown socket =
    wrap_io Lwt_ssl.ssl_shutdown socket

  module M_ugly_ssl_get_certificate = struct
    type ttt = Plain | SSL of Ssl.socket
    type lwt_ssl_socket = Lwt_unix.file_descr * ttt

    let get_certificate sslsock =
      begin match snd (Obj.magic sslsock : lwt_ssl_socket) with
      | Plain -> error (`not_an_ssl_socket)
      | SSL s ->
        Lwt.(
          catch
            (fun () ->
              Lwt_preemptive.detach Ssl.get_certificate s >>= fun cert ->
              return (Ok cert))
            (function
            | Ssl.Certificate_error ->
              return (Error `ssl_certificate_error)
            | e -> 
              return (Error (`io_exn e))))
      end
  end
  let tls_get_certificate = M_ugly_ssl_get_certificate.get_certificate



end

module Server = struct

  let tls_context ?ca_certificate (cert_file, key_file) =
    let open Ssl in
    try
      let c = create_context TLSv1 Server_context in
      use_certificate c cert_file key_file;
      set_cipher_list c "TLSv1";
      Option.iter ca_certificate (fun ca_cert ->
        set_verify c [ Verify_peer; ] None;
        set_verify_depth c 99;
        load_verify_locations c ca_cert "";
      );
      return c
    with e -> error (`tls_context_exn e)

  let server_socket ~port =
    let open Lwt_unix in
    try
      let fd = socket PF_INET SOCK_STREAM 6 in
      bind fd (ADDR_INET (Unix.Inet_addr.bind_any, port));
      listen fd port;
      return fd
    with
    | e -> error (`socket_creation_exn e)

  let tls_accept_loop ?check_client_certificate ssl_context ~port f =
    server_socket ~port:2000 >>= fun socket ->
    let handle_one accepted =
      Tls.accept (fst accepted) ssl_context >>= fun ssl_accepted ->
      dbg "Accepted (SSL)" >>= fun () ->
      begin match check_client_certificate with
      | Some ccc ->
        double_bind (Tls.tls_get_certificate ssl_accepted)
          ~error:(function
          | `ssl_certificate_error -> return (`invalid_client `wrong_certificate)
          | `not_an_ssl_socket | `io_exn _ as e -> error e)
          ~ok:(fun cert ->
            ccc cert >>= function
            | `ok -> return (`valid_client cert)
            | `expired -> return (`invalid_client (`expired_certificate cert))
            | `certificate_not_found ->
              return (`invalid_client (`certificate_not_found cert))
            | `revoked -> return (`invalid_client (`revoked_certificate cert)))
      | None -> return `anonymous_client
      end
      >>= fun client ->
      f ssl_accepted client
    in
    let rec accept_loop c =
      dbg "Accepting #%d (unix)" c >>= fun () ->
      wrap_io (Lwt_unix.accept_n socket) 10
      >>= fun (accepted_list, potential_exn) ->
      dbg "Accepted %d connections (unix)%s" (List.length accepted_list)
        (Option.value_map ~default:"" potential_exn
           ~f:(fun e -> sprintf ", Exn: %s" (Exn.to_string e)))
      >>= fun () ->
      accept_loop (c + 1) |! Lwt.ignore_result;
      Lwt.(
        Lwt_list.map_p handle_one accepted_list
        >>= fun res_l ->
        List.iter res_l (function
        | Ok () -> ()
        | Error e -> eprintf "THERE WERE ERRORS IN THE HANDLER");
        return (Ok ()))
    in
    let c = Lwt_condition.create () in
    accept_loop 0
    >>= fun () ->
    wrap_io Lwt_condition.wait c
    >>= fun () ->
    dbg "END OF ACCEPT_LOOP"


end

module Client = struct

  let tls_context ?verification_policy kind =
    let open Ssl in
    begin
      try
        let c = create_context TLSv1 Client_context in
        begin match kind with
        | `anonymous -> ()
        | `with_certificate (cert, key) ->
          use_certificate c cert key
        end;
        set_cipher_list c "TLSv1";
        Option.iter verification_policy (function
        | `verify_server -> 
          Ssl.set_verify_depth c 99;
          set_verify c [Verify_peer] (Some client_verify_callback);
        | `allow_self_signed -> ()
        );
        return c
      with e -> error (`tls_context_exn e)
    end

  type connection_specification = [
  | `tls of
      [ `anonymous | `with_certificate of string * string ]
    * [ `verify_server | `allow_self_signed ]
  | `plain
  ]
    
  let unix_connect sockaddr =
    let socket =
      Lwt_unix.(
        try
          let fd = socket PF_INET SOCK_STREAM 0 in
          fd
        with
        | Unix.Unix_error (e, s, a) as ex ->
          eprintf "Unix.Unix_error: %s %s %s\n%!" (Unix.error_message e) s a;
          raise ex
      ) in
    wrap_io (Lwt_unix.connect socket) sockaddr
    >>= fun () ->
    return socket
      
  class ['a] connection inchan outchan tls_socket =
  object
    method in_channel: Lwt_io.input_channel = inchan
    method out_channel: Lwt_io.output_channel = outchan
    method shutdown : (unit, [> `io_exn of exn ] as 'a) Sequme_flow.t
      = Tls.tls_shutdown tls_socket
  end

  let connect ~address specification =
    begin match specification with
    | `plain ->
      unix_connect address >>= fun unix_socket_fd ->
      let socket_fd = Lwt_ssl.plain unix_socket_fd in
      let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
      let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
      return (new connection inchan outchan socket_fd)
    | `tls (connection_type, verification_policy) ->
      unix_connect address >>= fun unix_socket_fd ->
      tls_context ~verification_policy connection_type >>= fun tls_context ->
      Tls.tls_connect unix_socket_fd tls_context >>= fun socket_fd ->
      let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
      let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
      return (new connection inchan outchan socket_fd)
    end


      
end
