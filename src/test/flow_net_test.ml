
open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_net = Sequme_flow_net
  
let log fmt =
  ksprintf (fun s ->
    let indented =
      s |! String.split ~on:'\n' |! String.concat ~sep:"\n         " in
    wrap_io (Lwt_io.eprintf "𝕱☏-test: %s\n%!") indented) fmt

let failwithf fmt =
  ksprintf (fun s -> error (`flow_ca_error s)) fmt

let main () =
  log "Start!\n%s" Time.(now () |! to_string)
  
  
let () =
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
    end
  end
