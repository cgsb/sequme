
module OCamlbuild_with_findlib = struct
  (** OCamlbuild extension, copied from
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall
    *)
  open Ocamlbuild_plugin

  (* these functions are not really officially exported *)
  let run_and_read =
    Ocamlbuild_pack.My_unix.run_and_read

  let blank_sep_strings =
    Ocamlbuild_pack.Lexers.blank_sep_strings

  let split s ch =
    let x =
      ref []
    in
    let rec go s =
      let pos =
        String.index s ch
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x

  let split_nl s = split s '\n'

  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"

      | After_rules ->

          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";

          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

      | _ ->
          ()

end

open Ocamlbuild_plugin
open Printf
module List = ListLabels

let sequme_version = "0.2"
let sequme_description = "NYU Bioinformatics Support Library"

let sequme_dependencies = [
  "threads";
  "core";
  "core_extended";
  "pgocaml";
  "biocaml";
  "lwt.ssl";
  "lwt.preemptive";
]

let sequme_build_packages = sequme_dependencies @ [
  "sexplib.syntax";
]
let sequme_modules = [
  "Sequme";
  "Sequme_internal_pervasives";
  "Sequme_bowtie";
  "Sequme_cuffdiff";
  "Sequme_cufflinks";
  "Sequme_illumina";
  "Sequme_macs";
  "Sequme_pbs";
  "Sequme_read_type";
  "Sequme_tophat";
  "Sequme_syscall";
  "Sequme_postgres";
  "Sequme_pgpass";
  "Sequme_doc_syntax";
  "Sequme_flow_monad";
  "Sequme_flow";
  "Sequme_flow_list";
  "Sequme_flow_sys";
  "Sequme_flow_io";
  "Sequme_flow_certificate_authority";
  "Sequme_flow_net";
  "Sequme_flow_app_util";
  "Sequme_psql";
]
let tags =
  "syntax_camlp4o" ::
  List.map (sprintf "pkg_%s") sequme_build_packages

let build_dispatch e =
  tag_any tags;
  let put_modules_in_file name =
    rule (sprintf "Create %s" name) ~prod:name
      begin fun _ _ ->
        Seq [
          Cmd (S [A "mkdir"; A "-p"; A (Filename.dirname name)]);
          Echo (sequme_modules, name);
        ]
      end;
  in
  put_modules_in_file "src/lib/sequme.mllib";
  rule "sequme_lib"
    ~deps:["src/lib/sequme.cma"; "src/lib/sequme.cmxa"; "src/lib/sequme.cmxs"]
    ~prod:"sequme_lib"
    begin fun _ _ ->
      ocaml_lib ~dir:"src/lib" "src/lib/sequme";
      Nop
    end;
  let sequme_meta_file = "src/lib/META" in
  rule "Make META" ~deps:[] ~prod:sequme_meta_file
    begin fun _ _ ->
      let meta_file = [
        sprintf "version = %S\n" sequme_version;
        sprintf "description = %S\n" sequme_description;
        sprintf "requires = %S\n" (String.concat " " sequme_dependencies);
        "archive(byte) = \"sequme.cma\"\n\
         archive(byte, plugin) = \"sequme.cma\"\n\
         archive(native) = \"sequme.cmxa\"\n\
         archive(native, plugin) = \"sequme.cmxs\"\n\
         exists_if = \"sequme.cma\"\n";
      ] in
      Echo (meta_file,  "src/lib/META")
    end;

  let tests = ref [] in
  let register_test base =
    let dot_ml = sprintf "src/test/%s.ml" base in
    let dot_native = sprintf "src/test/%s.native" base in
    tag_file dot_ml ["use_sequme"; ];
    tag_file dot_native ["use_sequme"];
    tests := dot_native :: !tests
  in
  register_test "flow_examples";
  register_test "flow_ca";
  register_test "flow_net_test";

  (* The `build` pseudo-target means `sequme_lib` and all the tests: *)
  rule "build"
    ~deps:("sequme_lib" :: !tests)
    ~prod:"build"
    begin fun _ _ -> Nop end;

  rule "install"
    ~deps:["sequme_lib"; sequme_meta_file; "uninstall"]
    ~prod:"install"
    begin fun _ _ ->
      let to_install =
        List.map ["cmi"; "cmo"; "cmx"; "a"; "o"; "cma"; "cmxa"; "cmxs"]
          ~f:(fun ext -> Sh (sprintf "src/lib/*.%s" ext))
      in
      Seq [
        Cmd (S (A "ocamlfind" :: A "install" :: A "sequme" :: P sequme_meta_file
            :: to_install));
      ]
    end;
  rule "uninstall" ~deps:[] ~prod:"uninstall"
    begin fun _ _ -> Cmd (S [A "ocamlfind"; A "remove"; A "sequme"]) end;
  ()



let () =
  dispatch (fun e ->
    OCamlbuild_with_findlib.dispatch e;
    match e with
    | After_rules ->
      build_dispatch ();
    | _ -> ()

  )
