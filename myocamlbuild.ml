(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let mydispatch = function
  | After_options ->
      Options.ocamldoc := S [A"ocamlfind"; 
                             A"ocamldoc";
                             A"-keep-code";
                             A"-charset"; A"utf-8";
                             (* A"-css-style"; A"../doc/style.css"; *)
                             A"-t"; A "The Sequme Library";
                             A"-colorize-code"]
  | _ -> ()

let () =
  dispatch (fun e ->
            dispatch_default e;
            mydispatch e)
