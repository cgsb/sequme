open Core.Std
open Sequme_flow

module Test = struct


  let log fmt =
    ksprintf (fun s -> eprintf "TLOG: %s\n%!" s) fmt

  let tests =
    (ref []:
       (string,
        unit -> 
        (unit, [ `io_exn of Core.Std.Exn.t ]) Sequme_flow.t) Core.Std.List.Assoc.t ref)
  let add n m =
    tests := (n, m) :: !tests

  let run_flow m =
    try
      begin match Lwt_main.run (m ()) with
      | Ok () -> log "End: Ok"
      | Error (`io_exn e) -> log "End: Error: %s" (Exn.to_string e)
      end
    with
    | e -> log "End: Exception: %s" (Exn.to_string e)
      
      
  let run_test n =
    match List.Assoc.find !tests n with
    | None -> log "ERROR: no test named %S" n
    | Some m -> (log "STARTING TEST %S" n; run_flow m)
      
  let run_all_tests () =
    log "Running all tests";
    List.iter (List.rev !tests) (fun (n, m) -> log "STARTING TEST %S" n; run_flow m)

end


let () =
  Test.add "basic" (fun () ->
    Test.log "Started !";
    return ())
    
let () =
  let on_item d =
    Test.log "%d before sleep" d;
    wrap_io Lwt_unix.sleep 0.3
    >>= fun () ->
    Test.log "%d after sleep" d;
    return ()
  in
  let items = [ 11;22;33;44; ] in
  Test.add "lists" (fun () ->
    map_sequential items on_item
    >>= fun _ ->
    map_concurrent items on_item
    >>= fun _ ->
    return ())

let () =
  Test.add "options" (fun () ->
    let show = function
      | None -> Test.log "  It's None"
      | Some s -> Test.log "  It's Some %S" s in
    let f s = return (String.uppercase s) in
    map_option None ~f >>| show >>= fun () ->
    map_option (Some "thing") ~f >>| show >>= fun () ->
    return ())


let () =
  let open Sequme_flow_sys in
  let test_syscmd s =
    let spiced = sprintf "%s >> /tmp/flow_examples.log 2>&1" s in
    Test.log "  Running: %S" spiced;
    double_bind (system_command spiced)
      ~ok:(fun () -> Test.log "  -> Success"; return ())
      ~error:(function
      | `io_exn e ->
        Test.log " -> IO Exn: %s" (Exn.to_string e); return ()
      | `system_command_error (s, err) ->
        Test.log "  -> system_command_error: %s"
          (match err with
          | `exn e -> Exn.to_string e
          | `exited n -> sprintf "exited with %d" n
          | `signaled n -> sprintf "signaled with %d" n
          | `stopped n -> sprintf "stopped: %d" n);
        return ())
  in
  let test_write_file file ~content =
    Test.log "  Writing %s (%d bytes)" file (String.length content);
    double_bind (write_file file ~content)
      ~ok:(fun () -> Test.log "  -> OK"; return ())
      ~error:(function
      | `io_exn e -> Test.log " -> IO Exn: %s" (Exn.to_string e); return ()
      | `write_file_error (_, e) ->
        Test.log "  -> write_file_error: %s" (Exn.to_string e); return ())
  in
  let test_read_file file =
    Test.log "  Reading %s" file;
    double_bind (read_file file)
      ~ok:(fun s -> Test.log "  -> read %d bytes" (String.length s); return ())
      ~error:(function
      | `io_exn e -> Test.log " -> IO Exn: %s" (Exn.to_string e); return ()
      | `read_file_error (_, e) ->
        Test.log "  -> read_file_error: %s" (Exn.to_string e); return ())
  in
  Test.add "flow_sys" (fun () ->
    test_syscmd "ls" >>= fun () ->
    test_syscmd "command_not_found__" >>= fun () ->
    test_syscmd "cat /etc/shadow" >>= fun () ->
    test_read_file "/tmp/flow_examples.log" >>= fun () ->
    test_write_file "/tmp/flow_examples.log" ~content:"" >>= fun () ->
    test_read_file "/tmp/flow_examples.log" >>= fun () ->
    test_read_file "/etc/passwd" >>= fun () ->
    test_read_file "/etc/shadow" >>= fun () ->
    test_write_file "/etc/passwd" ~content:"" >>= fun () ->
    test_write_file "/etc/shadow" ~content:"" >>= fun () ->
    test_read_file "/tmplskjdgfldkjgjfdsgfdsghf123" >>= fun () ->
    test_write_file "/tmpls/kjdgfldkjgjfdsgfdsghf123" ~content:"" >>= fun () ->
    return ())

let () =
  let open Sequme_flow_sys in
  let substring s m =
    if String.length s <= m then s else String.sub s 0 m in
  let pad s m = sprintf "%s%s" s (String.make (m - String.length s) '0') in
  let print_error err =
    begin match err with
    | `io_exn e -> Test.log "  I/O Exn: %s" (Exn.to_string e) 
    | `bin_recv (`error e) -> Test.log "  Unknown recv error: %s" (Exn.to_string e)
    | `bin_recv (`wrong_length (c, s)) ->
      Test.log "  Wrong-length recv error: %d Vs %d (%S)"
        c (String.length s) (substring s 20)
    | `bin_send (`error e) -> Test.log "  Unknown send error: %s" (Exn.to_string e)
    | `bin_send (`message_too_long s) ->
      Test.log "  Message too long: %S" (substring s 20)
    end;
    return () in
  let with_timeout m r =
    Lwt.pick [m ;
              wrap_io Lwt_unix.sleep 1.
              >>= fun () ->
              Test.log "  TIMEOUT!";
              return r] 
  in 
  let read_and_write ic oc s =
    map_concurrent [`write s; `read] (function
    | `write s ->
      bin_send oc s
    | `read ->
      with_timeout (bin_recv ic) "NOTHING"
      >>= fun s ->
      Test.log "  Recv: %S" (substring s 20);
      return ())
    >>= fun _ ->
    return ()
  in
  Test.add "bin_send/recv" (fun () ->
    let ic, oc = Lwt_io.pipe ~buffer_size:4242 () in
    bind_on_error (read_and_write ic oc "booh") print_error
    >>= fun () ->
    bind_on_error (read_and_write ic oc "") print_error
    >>= fun () ->
    bind_on_error (read_and_write ic oc (pad "good string " 10_000_000)) print_error
    >>= fun () ->
    bind_on_error (read_and_write ic oc (pad "bad string " 100_000_000)) print_error
  )
    
let () =
  match Array.to_list Sys.argv with
  | [] | _ :: [] -> Test.log "nothing to do"
  | exec :: ["-all"] -> Test.run_all_tests ()
  | exec :: l ->
    List.iter l Test.run_test
      
