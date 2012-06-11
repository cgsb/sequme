(* concat this file after the OASIS-generated myocamlbuild.ml *)

dispatch begin (fun d ->
  dispatch_default d; (* Call the OASIS-generated dispatch. *)
  (function 
  | After_options ->
    Options.ocamldoc := S [A"ocamlfind"; 
                           A"ocamldoc";
                           A"-keep-code";
                           A"-charset"; A"utf-8";
                           (* A"-css-style"; A"../doc/style.css"; *)
                           A"-t"; A "The Sequme Library";
                           A"-colorize-code"]
  | _ -> ()) d) (* Add one more dispatch option. *)
end;;
