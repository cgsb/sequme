open Core.Std
open Sequme_flow
  
let  write_file file ~content =
  wrap_io ~on_exn:(fun e -> `write_file_error (file, e))
    Lwt_io.(fun () -> with_file ~mode:output file (fun i -> write i content)) ()

let read_file file =
  wrap_io ~on_exn:(fun e -> `read_file_error (file, e))
    Lwt_io.(fun () -> with_file ~mode:input file (fun i -> read i)) ()

let system_command s =
  wrap_io
    ~on_exn:(fun exn -> `system_command_error (s, `exn exn))
    Lwt_unix.system s
  >>= fun ret ->
  begin match ret with
  | Lwt_unix.WEXITED 0 -> return ()
  | Lwt_unix.WEXITED n -> error (`system_command_error (s, `exited n))
  | Lwt_unix.WSIGNALED n -> error (`system_command_error (s, `signaled n))
  | Lwt_unix.WSTOPPED n -> error (`system_command_error (s, `stopped n))
  end

let max_message_length = 10_000_000
  
let bin_send oc msg =
  if String.length msg > max_message_length then
    error (`bin_send (`message_too_long msg))
  else
    wrap_io ~on_exn:(fun e -> `bin_send (`error e))
      Lwt.(fun s ->
        Lwt_io.BE.write_int oc (String.length s) >>= fun () ->
        Lwt_io.write oc s)
      msg

let bin_recv ic =
  wrap_io ~on_exn:(fun e -> `bin_recv (`error e))
    Lwt.(fun () ->
      Lwt_io.BE.read_int ic >>= fun c ->
      begin if max_message_length >= c && c > 0 then (
        let s = String.make c 'B' in
        Lwt_io.read_into_exactly ic s 0 c >>= fun () ->
        return s
          (* ~count:(min c max_message_length) ic *)
      ) else
        return ("")
      end
      >>= fun s ->
      return (c,s)
    ) ()
  >>= fun (c, s) ->
  if String.length s <> c then
    error (`bin_recv (`wrong_length (c, s)))
  else
    return s
      

