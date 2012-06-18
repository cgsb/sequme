
open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_net = Sequme_flow_net
module Flow_CA = Sequme_flow_certificate_authority
  
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

let print_errors_and_unit name m : unit Lwt.t =
  Lwt.(m >>= function
  | Ok () -> return ()
  | Error e ->
    begin match e with
   | `io_exn e -> 
     Lwt_io.eprintf "%s: I/O Exn: %s\n" name (Exn.to_string e) 
   | `tls_context_exn e ->
     Lwt_io.eprintf "%s: TLS-Context Exn: %s\n" name (Exn.to_string e) 
   | `socket_creation_exn e ->
     Lwt_io.eprintf "%s: TLS-Socket Exn: %s\n" name (Exn.to_string e) 
   | `bin_send (`message_too_long _) ->
     Lwt_io.eprintf "%s: Bin-send: message too long\n" name
   | `bin_send (`exn e) ->
     Lwt_io.eprintf "%s: Bin-send Exn: %s\n" name (Exn.to_string e) 
    end)

let client (client1_name, client1_cert_key) =
  logc "Start!"
  >>= fun () ->
  sleep 1.0
  >>= fun () ->
  Flow_net.Client.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 2000))
    (`tls (`anonymous, `allow_self_signed))
  >>= fun connection ->
  logc "Anonymously Connected (unix)" >>= fun () ->
  Sequme_flow_io.bin_send connection#out_channel "Hello !!"
  >>= fun () ->
  sleep 4. >>= fun () ->
  connection#shutdown
  >>= fun () ->
  logc "Disconnected." >>= fun () ->
  Flow_net.Client.connect
    ~address:Unix.(ADDR_INET (Inet_addr.localhost, 2000))
    (`tls (`with_certificate client1_cert_key, `allow_self_signed))
  >>= fun connection ->
  logc "Connected as %s (unix)" client1_name >>= fun () ->
  ksprintf (Sequme_flow_io.bin_send connection#out_channel)
    "Hello, I'm %S !!" client1_name
  >>= fun () ->
  sleep 4. >>= fun () ->
  connection#shutdown
  >>= fun () ->
  logc "Disconnected." >>= fun () ->
  logc "End."

let server name ca cert_key =
  logs "Start!" >>= fun () ->
  Flow_net.Server.tls_context
    ~ca_certificate:(Flow_CA.ca_certificate_path ca) cert_key
  >>= fun tls_context ->
  let check_client_certificate c =
    logs "check_client_certificate:\n  Issuer: %s\n  Subject: %s"
      (Ssl.get_issuer c) (Ssl.get_subject c) >>= fun () ->
    Flow_CA.check_certificate ca c
  in
  let print_incomming_message inchan =
    bind_on_error (Sequme_flow_io.bin_recv inchan) (function
    | `bin_recv (`exn e) ->
      logs "ERROR: bin-recv: %s (Ssl: %s)" (Exn.to_string e)
        (Ssl.get_error_string ())
      >>= fun () ->
      return "NOTHING"
    | `bin_recv (`wrong_length (l, s)) ->
      logs "ERROR: bin-recv: %d" l
      >>= fun () ->
      return "NOTHING")
    >>= fun msg ->
    logs "Received: %S from client." msg
  in
  Flow_net.Server.tls_accept_loop ~check_client_certificate tls_context ~port:2000
    (fun client_socket client_kind ->
      (* let ouchan = Lwt_ssl.out_channel_of_descr client_socket in *)
      let inchan = Lwt_ssl.in_channel_of_descr client_socket in
      begin match client_kind with
      | `invalid_client `wrong_certificate ->
        logs "The client has a wrong certificate\n(SSL: %s)" 
            (Ssl.get_error_string ())
        >>= fun () ->
        print_incomming_message inchan
      | `invalid_client (`expired (n, t)) ->
        logs "The client %S has a certificate expired since %s" n Time.(to_string t)
      | `invalid_client (`revoked (n, t)) ->
        logs "The client %S has a certificate revoked since %s" n Time.(to_string t)
      | `invalid_client (`not_found n) ->
        logs "The client has a not-found certificate"
      | `anonymous_client ->
        logs "Reading from anonymous_client..." >>= fun () ->
        print_incomming_message inchan
      | `valid_client name ->
        logs "Reading from authenticated client: %S" name >>= fun () ->
        print_incomming_message inchan
      end)
  >>= fun () ->
  logs "End."

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
    
let main () =
  logt "Start!\n%s" Time.(now () |! to_string) >>= fun () ->
  certificates ()
  >>= fun (ca, (server_name, server_cert_key), client1) ->
  wrap_io Lwt.pick [client client1 |! print_errors_and_unit "client";
                    server server_name ca server_cert_key
                    |! print_errors_and_unit "server"]
  
  
let () =
  Flow_net.Tls.init ();
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
    end
  end
