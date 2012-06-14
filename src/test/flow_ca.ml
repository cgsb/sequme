
open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_CA = Sequme_flow_certificate_authority

let log fmt =
  ksprintf (fun s ->
    let indented = s |! String.split ~on:'\n' |! String.concat ~sep:"\n     " in
    wrap_io (Lwt_io.eprintf "𝕱ca: %s\n%!") indented) fmt

let failwithf fmt =
  ksprintf (fun s -> error (`flow_ca_error s)) fmt

let do_establishment path =
  let ca =
    Flow_CA.create
      ~openssl_command:"openssl"
      ~dn_country:"US"
      ~dn_province:"NY"
      ~dn_city:"New York"
      ~dn_org:"Sequme Test Ltd."
      ~dn_orgunit:"Dept. of Bio."
      ~dn_email:"sequme@example.com"
      ~rsa_key_size:4096
      ~default_validity:3650
      ~ca_filename_prefix:"sequme_flow_certificate_authority"
      ~ca_cn:"Sequme_CA"
      path  in
  Flow_CA.establish ca

let server path name =
  Flow_CA.load path >>= fun ca ->
  Flow_CA.make_server_certificate ca ~name >>= fun () ->
  begin match Flow_CA.server_crtkey_path ca ~name with
  | None -> failwithf "Cannot find the certificate just created"
  | Some p -> log "Created: %s" p
  end
  >>= fun () ->
  begin match Flow_CA.server_certificate_and_key_paths ca ~name with
  | None -> failwithf "Cannot find the certificate just created"
  | Some (crt, key) -> log "And:\n%s\n%s" crt key
  end

let server_info path name =
  Flow_CA.load path >>= fun ca ->
  begin match Flow_CA.server_history ca ~name with
  | None -> log "Server %S not found." name
  | Some l ->
    log "Server %S:\n%s" name
      (l |! List.rev |! List.map ~f:(fun (cn, cert_hist) ->
        sprintf "Cert %s:\n%s" cn
          (cert_hist |! List.rev |! List.map ~f:(function
            | `created t -> sprintf "  created %s" Time.(to_string t)
            | `revoked t -> sprintf "  revoked %s" Time.(to_string t))
          |! String.concat ~sep:"\n"))
      |! String.concat ~sep:"\n")
  end
  
let main () =
  begin match Array.to_list Sys.argv with
  | [] | [_] ->
    log "Nothing to do.\n\
         usage: flow_ca <cmd> <args>\n\
         flow_ca establish <path>\n\
         flow_ca server <path> <common-name>"
  | exec :: "establish" :: path :: [] -> do_establishment path  
  | exec :: "server" :: path :: name :: [] -> server path name
  | exec :: "server-info" :: path :: name :: [] -> server_info path name
  | exec :: l ->
    log "Don't know what to do with [%s]"
      (l |! List.map ~f:(sprintf "%S") |! String.concat ~sep:", ")
  end


  
let () =
  begin match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    begin match e with
    | `io_exn e -> eprintf "End with ERROR: %s\n" (Exn.to_string e)
    | `system_command_error (cmd, status) ->
      eprintf "End with ERROR: SYS-COMMAND %S failed\n" (cmd)
    | `parse_config_error e ->
      eprintf "End with ERROR: Parsing-config %s" (Exn.to_string e)
    | `read_file_error (file, exn) ->
      eprintf "End with ERROR: Read-file %S failed: %s\n" file (Exn.to_string exn)
    | `write_file_error (file, exn) ->
      eprintf "End with ERROR: Write-file %S failed: %s\n" file (Exn.to_string exn)
    | `flow_ca_error s -> eprintf "End with ERROR: %s\n" s
    end
  end
  
