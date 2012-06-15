
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
     
    end)

let client () =
  logc "Start!"
  >>= fun () ->
  wrap_io Lwt_unix.sleep 1.0
  >>= fun () ->
  Flow_net.Client.tls_context ~verification_policy:`ok_self_signed `anonymous
    (* Option.(value_map with_auth ~default:`anonymous *)
              (* ~f:(fun (_, c) -> `with_pem_certificate c)) *)
  >>= fun ssl_context ->
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
  wrap_io (Lwt_unix.connect socket) Unix.(ADDR_INET (Inet_addr.localhost, 2000))
  >>= fun () -> 
  logc "Connected (unix)" >>= fun () ->
  Flow_net.Tls.tls_connect socket ssl_context >>= fun socket ->
  (* let inchan = Lwt_ssl.in_channel_of_descr  socket in *)
  (* let ouchan = Lwt_ssl.out_channel_of_descr socket in *)
  logc "Connected (ssl), writing" >>= fun () ->
  Flow_net.Tls.tls_shutdown socket >>= fun () ->
  logc "Disconnected (ssl)" >>= fun () ->
  wrap_io Lwt_unix.sleep 2. >>= fun () ->

  logc "End."

let server name cert_key =
  logs "Start!" >>= fun () ->
  Flow_net.Server.tls_context cert_key >>= fun tls_context ->
  Flow_net.Server.tls_accept_loop tls_context ~port:2000
    (fun client_socket client_kind ->
      (* let inchan = Lwt_ssl.in_channel_of_descr client_socket in *)
      (* let ouchan = Lwt_ssl.out_channel_of_descr client_socket in *)
      begin match client_kind with
      | `invalid_client `wrong_certificate ->
        logs "The client has a wrong certificate"
      | `invalid_client (`expired_certificate _) ->
        logs "The client has an expired certificate"
      | `invalid_client (`revoked_certificate _) ->
        logs "The client has a revoked certificate"
      | `invalid_client (`certificate_not_found _) ->
        logs "The client has a not-found certificate"
      | `anonymous_client ->
        logs "Reading..." >>= fun () ->
        return ()
      | `valid_client cert ->
        (* let login = Certificate_authority.login_of_cert cert in *)
        (* logs "Reading... from %s" (Option.value ~default:"NOT-NAMED" login) *)
        logs "Reading from authenticated client"
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
  Flow_CA.make_server_certificate ca ~name >>= fun () ->
  Flow_CA.server_certificate_and_key_paths ca ~name |! of_result
  >>= fun (crt, key) ->
  logt "Certification of %s:\n%s\n%s" name crt key
  >>= fun () ->
  return (name, (crt, key))
    
let main () =
  logt "Start!\n%s" Time.(now () |! to_string) >>= fun () ->
  certificates ()
  >>= fun (server_name, server_cert_key) ->
  wrap_io Lwt.pick [client () |! print_errors_and_unit "client";
                    server server_name server_cert_key
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
    | `server_not_found s ->
      eprintf "End with ERROR: Server_Not_Found: %S" s
    | `certificate_revoked (n, t) ->
      eprintf "End with ERROR: Certificate_Revoked: %S on %s" n Time.(to_string t)
    end
  end
