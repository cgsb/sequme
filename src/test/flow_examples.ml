open Core.Std
open Sequme_flow

module Test = struct


  let log fmt =
    ksprintf (fun s -> eprintf "TLOG: %s\n%!" s) fmt

  let tests =
    (ref []:
       (string,
        (unit, [ `io_exn of Core.Std.Exn.t ]) Sequme_flow.t) Core.Std.List.Assoc.t ref)
  let add n m =
    tests := (n, m) :: !tests

  let run_flow m =
    try
      begin match Lwt_main.run m with
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
  Test.add "basic"
    (wrap_io Lwt_unix.sleep 1.
     >>= fun () ->
     Test.log "Started !";
     return ())


let () =
  match Array.to_list Sys.argv with
  | [] | _ :: [] -> Test.log "nothing to do"
  | exec :: ["-all"] -> Test.run_all_tests ()
  | exec :: l ->
    List.iter l Test.run_test
      
