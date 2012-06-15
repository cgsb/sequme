open Core.Std
open Sequme_flow
  
let  write_file file ~content =
  catch_io () ~f:Lwt_io.(fun () ->
    with_file ~mode:output file (fun i -> write i content))
  |! bind_on_error ~f:(fun e -> error (`write_file_error (file, e)))

let read_file file =
  catch_io () ~f:Lwt_io.(fun () ->
    with_file ~mode:input file (fun i -> read i))
  |! bind_on_error ~f:(fun e -> error (`read_file_error (file, e)))

let system_command s =
  bind_on_error ~f:(fun e -> error (`system_command_error (s, `exn e)))
    (catch_io () ~f:Lwt_io.(fun () -> Lwt_unix.system s))
  >>= fun ret ->
  begin match ret with
  | Lwt_unix.WEXITED 0 -> return ()
  | Lwt_unix.WEXITED n -> error (`system_command_error (s, `exited n))
  | Lwt_unix.WSIGNALED n -> error (`system_command_error (s, `signaled n))
  | Lwt_unix.WSTOPPED n -> error (`system_command_error (s, `stopped n))
  end

let sleep f =
  wrap_io Lwt_unix.sleep f

    
