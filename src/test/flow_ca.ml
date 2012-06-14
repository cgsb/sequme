
open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_CA = Sequme_flow_certificate_authority

let log fmt =
  ksprintf (fun s ->
    let indented = s |! String.split ~on:'\n' |! String.concat ~sep:"\n     " in
    wrap_io (Lwt_io.eprintf "𝕱ca: %s\n%!") indented) fmt

let do_establishment path =
  let ca =
    Flow_CA.create
      ~openssl_command:"openssl"
      ~dn_country:"US"
      ~dn_province:"NY"
      ~dn_city:"New York"
      ~dn_org:"Sequme Test Ltd."
      ~dn_orgunit:"Dept. of Bio."
      ~dn_cn:"DEFAULT_CN"
      ~dn_email:"sequme@example.com"
      ~rsa_key_size:4096
      ~default_validity:3650
      ~ca_filename_prefix:"sequme_flow_certificate_authority"
      ~ca_cn:"Sequme_CA"
      path  in
  Flow_CA.establish ca

let main () =
  begin match Array.to_list Sys.argv with
  | [] | [_] ->
    log "Nothing to do.\n\
         usage: flow_ca <cmd> <args>\n\
         flow_ca establish <path>"
  | exec :: "establish" :: path :: [] ->
    do_establishment path  
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
    | `write_file_error (file, exn) ->
      eprintf "End with ERROR: Write-file %S failed: %s\n"
        file (Exn.to_string exn)
    end
  end
  
