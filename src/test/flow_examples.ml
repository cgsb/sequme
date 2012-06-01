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
  match Array.to_list Sys.argv with
  | [] | _ :: [] -> Test.log "nothing to do"
  | exec :: ["-all"] -> Test.run_all_tests ()
  | exec :: l ->
    List.iter l Test.run_test
      
