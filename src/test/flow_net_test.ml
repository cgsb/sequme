(*

  This program is a test for the Flow_net module.

  It launches 3 servers:
  - TCP on port 4001
  - TCP + TLS on port 4002
  - TCP + TLS + Client-certificates on port 4003
  The servers just log incoming messages and reply to them once.
  
  Then successive clients connect, send messages, and disconnect.

*)
open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_net = Sequme_flow_net
module Flow_CA = Sequme_flow_certificate_authority
  
(* -------------------------------------------------------------------------- *)
(* Logging functions/Utilities: *)

let log_any whoami fmt =
  let prompt = sprintf "𝕱☏-%s: " whoami in
  let prompt_length = String.length prompt - 5 in
  ksprintf (fun s ->
    let indented =
      s |! String.split ~on:'\n'
      |! String.concat ~sep:(sprintf "\n%s" String.(make prompt_length ' '))
    in
    wrap_io (Lwt_io.eprintf "%s%s\n%!" prompt) indented) fmt

let logt fmt = log_any "test" fmt
let logc fmt = log_any "client" fmt
let logs fmt = log_any "server" fmt

let failwithf fmt =
  ksprintf (fun s -> error (`flow_ca_error s)) fmt

let cmd fmt =
  ksprintf (fun s ->
    log_any "syscmd" "%s" s >>= fun () ->
    system_command s)
    fmt

(* -------------------------------------------------------------------------- *)
(* Receive a message on the connection and reply to it. *)
let echo_server connection =
  bind_on_error (Sequme_flow_io.bin_recv connection#in_channel) (function
  | `bin_recv (`exn e) ->
    logs "ERROR: bin-recv: %s (Ssl: %s)" (Exn.to_string e) (Ssl.get_error_string ())
    >>= fun () ->
    return "NOTHING"
  | `bin_recv (`wrong_length (l, s)) ->
    logs "ERROR: bin-recv: %d" l
    >>= fun () ->
    return "NOTHING")
  >>= fun msg ->
  logs "Received: %S from client." msg
  >>= fun () ->
  Sequme_flow_io.bin_send connection#out_channel (sprintf "%s back ..." msg)


(* -------------------------------------------------------------------------- *)
(* For the authenticated case, display the 'kind' of client and then
   do the 'echo'. *)
let client_info_and_echo connection client_kind =
  begin match client_kind with
  | `invalid_client `wrong_certificate ->
    logs "The client has a wrong certificate\n(SSL: %s)" (Ssl.get_error_string ())
  | `invalid_client (`expired (n, t)) ->
    logs "The client %S has a certificate expired since %s" n Time.(to_string t)
  | `invalid_client (`revoked (n, t)) ->
    logs "The client %S has a certificate revoked since %s" n Time.(to_string t)
  | `invalid_client (`not_found n) ->
    logs "The client has a not-found certificate"
  | `anonymous_client ->
    logs "Reading from anonymous_client..."
  | `valid_client name ->
    logs "Reading from authenticated client: %S" name
  end
  >>= fun () ->
  echo_server connection
  
  
(* -------------------------------------------------------------------------- *)
(* Launch the 3 servers.
   - 'name' is unused right now (it is the 'name' associated with the
     certification.
   - 'ca' is a Sequme_flow_certificate_authority.t
   - 'cert_key' is a pair of filenames (certificate, key)
*)
let servers name ca cert_key =
  Flow_net.plain_server ~port:4001 echo_server >>= fun () ->
  logs "Plain Server running on 4001" >>= fun () ->
  Flow_net.tls_server ~port:4002 ~cert_key echo_server >>= fun () ->
  logs "TLS Server running on 4002" >>= fun () ->
  Flow_net.authenticating_tls_server_with_ca
    ~ca ~port:4003 ~cert_key client_info_and_echo >>= fun () ->
  logs "Auth-TLS Server running on 4003" >>= fun () ->
  logs "End of preparation."

(* -------------------------------------------------------------------------- *)
(* As a client, send "Hello" and wait for the reply. *)
let send_and_recv connection =
  Sequme_flow_io.bin_send connection#out_channel "Hello !!"
  >>= fun () ->
  Sequme_flow_io.bin_recv connection#in_channel
  >>= fun msg ->
  logc "Got %S from server"  msg
  
(* -------------------------------------------------------------------------- *)
(* The client-side test. *)
let clients (client1_name, client1_cert_key) =
  logc "Starting."
  >>= fun () ->
  (* TCP (`plain) connection to localhost:4001 *) 
  Flow_net.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 4001)) (`plain)
  >>= fun connection ->
  logc "TCP-Connected on 4001 " >>= fun () ->
  send_and_recv connection
  >>= fun () ->
  connection#shutdown
  >>= fun () ->
  (* TCP + TLS connection to localhost:4002
     `anonymous -> do not use client-certificates
     `allow_self_signed -> do not check the server certificate *)
  Flow_net.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 4002))
    (`tls (`anonymous, `allow_self_signed))
  >>= fun connection ->
  logc "Anonymously Connected on 4002 " >>= fun () ->
  send_and_recv connection
  >>= fun () ->
  connection#shutdown
  >>= fun () ->
  logc "Disconnected." >>= fun () ->
  (* TCP + TLS connection to localhost:4003
     `with_certificate (file.crt, file.key) -> use client-certificates
     `allow_self_signed -> do not check the server certificate *)
  Flow_net.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 4003))
    (`tls (`with_certificate client1_cert_key, `allow_self_signed))
  >>= fun connection ->
  logc "Connected as %s on 4003" client1_name >>= fun () ->
  send_and_recv connection >>= fun () ->
  connection#shutdown
  >>= fun () ->
  (* TCP + TLS connection to localhost:4003
     `anonymous -> do not use client-certificates
     `allow_self_signed -> do not check the server certificate

     In that case the server expects client-certificates (:4003), so
     this client will appear as a (`invalid_client `wrong_certificate).
     The connection is still useful as `anonymous.
  *)
  Flow_net.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 4003))
    (`tls (`anonymous, `allow_self_signed))
  >>= fun connection ->
  logc "Anonymously Connected on 4003 " >>= fun () ->
  send_and_recv connection
  >>= fun () ->
  connection#shutdown
  >>= fun () ->
  logc "Disconnected."

(* ************************************************************************** *) 
(* Create a certificate authority and a bunch of certificates. *)
let certificates () =
  let ca_path = "/tmp/flow_net_test_ca" in
  cmd "rm -fr %s" ca_path >>= fun () ->
  let ca = Flow_CA.create ca_path in
  Flow_CA.establish ca
  >>= fun () ->
  let name = "Flow_net_server" in
  Flow_CA.make_certificate ca ~name ~kind:`server >>= fun () ->
  Flow_CA.certificate_and_key_paths ca ~name |! of_result
  >>= fun (crt, key) ->
  logt "Certification of %s:\n%s\n%s" name crt key
  >>= fun () ->
  let server = (name, (crt, key)) in
  let name = "Flow_net_client_1" in
  Flow_CA.make_certificate ca ~name ~kind:`client >>= fun () ->
  Flow_CA.certificate_and_key_paths ca ~name |! of_result
  >>= fun (crt, key) ->
  logt "Certification of %s:\n%s\n%s" name crt key
                >>= fun () ->
  let client1 = (name, (crt, key)) in
  return (ca, server, client1)
    
(* -------------------------------------------------------------------------- *)
(* The Main Lwt thread. *)
let main () =
  logt "Start!\n%s" Time.(now () |! to_string) >>= fun () ->
  certificates ()
  >>= fun (ca, (server_name, server_cert_key), client1) ->
  servers server_name ca server_cert_key
  >>= fun () ->
  clients client1
  
let () =
  Flow_net.init_tls ();
  begin match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    begin match e with
    | `io_exn e -> eprintf "End with ERROR: %s\n" (Exn.to_string e)
    | `system_command_error (cmd, status) ->
      eprintf "End with ERROR: SYS-COMMAND %S failed\n" (cmd)
    | `read_file_error (file, exn) ->
      eprintf "End with ERROR: Read-file %S failed: %s\n" file (Exn.to_string exn)
    | `write_file_error (file, exn) ->
      eprintf "End with ERROR: Write-file %S failed: %s\n" file (Exn.to_string exn)
    | `name_not_found s ->
      eprintf "End with ERROR: Server_Not_Found: %S" s
    | `certificate_revoked (n, t) ->
      eprintf "End with ERROR: Certificate_Revoked: %S on %s" n Time.(to_string t)
    | `socket_creation_exn exn ->
      eprintf "End with ERROR: Socket creation failed: %s\n" (Exn.to_string exn)
    | `tls_context_exn exn ->
      eprintf "End with ERROR: TLS context failed: %s\n" (Exn.to_string exn)
    | `bin_recv (`exn e) ->
      eprintf "End with ERROR: bin-recv: %s (Ssl: %s)"
        (Exn.to_string e) (Ssl.get_error_string ())
    | `bin_recv (`wrong_length (l, s)) ->
      eprintf "End with ERROR: bin-recv: %d" l
    | `bin_send (`exn e) ->
      eprintf "End with ERROR: bin-send: %s (Ssl: %s)"
        (Exn.to_string e) (Ssl.get_error_string ())
    | `bin_send (`message_too_long s) ->
      eprintf "End with ERROR: bin-send: %d" (String.length s)
    end
  end
