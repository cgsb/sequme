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
