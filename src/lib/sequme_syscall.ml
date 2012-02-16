open Sequme_std

(* Implementation follows code provided at
   http://rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let syscall ?path cmd =
  let env = (
    try match path with
      | None ->
          [|sprintf "PATH=%s" (Sys.getenv "PATH")|]
      | Some x ->
          [|sprintf "PATH=%s" x|]
    with Not_found -> [||]
  )
  in
  let ic, oc, ec = Unix.open_process_full cmd env in
  let open Buffer in
  let buf1 = create 96
  and buf2 = create 48 in
  (
    try while true do add_channel buf1 ic 1 done
    with IO.No_more_input -> ()
  );
  (
    try while true do add_channel buf2 ec 1 done
    with IO.No_more_input-> ()
  );
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  contents buf1, contents buf2, exit_status
