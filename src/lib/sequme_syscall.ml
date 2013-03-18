open Sequme_internal_pervasives

(* Implementation follows code provided at
   http://rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let syscall ?path cmd =
  let env = (
    try match path with
      | None ->
          [|sprintf "PATH=%s" (Sys.getenv_exn "PATH")|]
      | Some x ->
          [|sprintf "PATH=%s" x|]
    with Not_found -> [||]
  )
  in
  let ic, oc, ec = Caml.Unix.open_process_full cmd env in
  let open Buffer in
  let buf1 = create 96
  and buf2 = create 48 in
  (
    try while true do add_channel buf1 ic 1 done
    with End_of_file -> ()
  );
  (
    try while true do add_channel buf2 ec 1 done
    with End_of_file -> ()
  );
  let exit_status = Caml.Unix.close_process_full (ic, oc, ec) in
  contents buf1, contents buf2, exit_status


let check_exit_status ?process x =
  let p = match process with None -> "" | Some p -> p ^ ": " in
  Caml.Unix.(match x with
    | WEXITED 0 -> ()
    | WEXITED x -> failwith (sprintf "%sterminated with exit code (%d)" p x)
    | WSIGNALED x -> failwith (sprintf "%skilled by signal %d" p x)
    | WSTOPPED x -> failwith (sprintf "%sstopped by signal %d" p x)
  )
