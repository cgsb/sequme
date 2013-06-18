(*doc

MPI Tests
=========


Compilation & run:

    ocamlfind ocamlc -package core,sexplib.syntax,mpi -linkpkg  -annot \
        -syntax camlp4o -thread src/exp/mpi_tests.ml -o mpi_tests \
       && mpirun -np 3 ./mpi_tests ALL


To view this in HTML:

    caml2html -nf -charset UTF-8 src/exp/mpi_tests.ml \
      -ext "doc: pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"  \
      -ext "style: awk 'BEGIN {print \"<style>\"} { print } END {  printf \"</style>\" }'" \
      -ext "result: xargs sh -c | pandoc | awk  'BEGIN {print \"<div class=result_box>\"} { print } END {  printf \"</div>\" }'"

*)
(*style
body {width: 700px; margin: auto}
code, pre {background-color: #ddf }
.text_box {
  font-size: 90%;
  margin-left: 0px;
  margin-right: 0px;
  font-family: serif }

.result_box {
  margin-left: 10%; width: 80%;
  (* margin-right: 10%; *)
  font-size: 70%;background-color: #fee;
  font-family: sans-serif
  padding-left: 10px;
  padding-right: 10px;
  }
.result_box code, .result_box pre {background-color: #fee;}

.text_box code, .text_box pre {background-color: #ddd;}
.text_box pre, .result_box pre {
  margin-left: 10%; width: 80%;
  (* margin-right: 10%; *)
  white-space: pre-wrap;
  padding-left: 10px;
  padding-right: 10px;
  text-indent: 1px;
}
h1 { font-size: 300% }
h2 { font-size: 230% }
h3 { font-size: 170% }
h4 { font-size: 110% }
blockquote {background-color: #eee}
.text_tip p { display: inline; }
*)
(*doc

About the MPI Tutorial
----------------------

### Getting Things to Work

On debian, I apt-got *OpenMPI*:

    apt-get install openmpi-bin libopenmpi-dev

and then `opam install mpi`
(the [MPI-instructions] where wrong for my case).

[MPI-instructions]: http://www.citutor.org/resources/MPI-instructions.html

### More Docs

Using [mpi.mli] and [test.ml].

[mpi.mli]: https://forge.ocamlcore.org/scm/viewvc.php/trunk/mpi.mli?view=markup&revision=29&root=ocamlmpi
[test.ml]: https://forge.ocamlcore.org/scm/viewvc.php/trunk/test.ml?view=markup&revision=18&root=ocamlmpi

Utilities
---------

*)
open Core.Std
let say fmt = printf (fmt ^^ "\n%!")
let args = Array.to_list Sys.argv
let if_arg s f =
  if List.mem args "ALL" || List.mem args s then f ()
let make_say_item rank size fmt =
  ksprintf (fun s -> say "* `[%d/%d]`: %s" rank (size - 1) s) fmt
(*doc

Example 1
---------

Inspired by [2.7 A First Program: ProcessColors][tuto27].

Using `send` and `receive_status`; blocking calls, as unsafe as the
`Marshall` module.

Note: I don't know yet what a `tag` is (if this case if not `0` it
  does not work).

[tuto27]: http://www.citutor.org/content.php?cid=1091
*)
let ex1 = if_arg "example01" (fun () ->
    let size = Mpi.comm_size Mpi.comm_world in
    let rank = Mpi.comm_rank Mpi.comm_world in
    let say_item fmt = make_say_item rank size fmt in
    Mpi.(barrier comm_world);
    say_item "Go!";
    begin match rank with
    | 0 ->
      say_item "As The Master";
      let rec loop = function
      | 0 -> say_item "The Master: END";
      | n ->
        let (msg, rk, tag) =
          Mpi.(receive_status any_source any_tag comm_world) in
        begin match msg with
        | "aaa" ->
          say_item "The Master received from `[%d]` (tag: %d)" rk tag;
          loop (n - 1)
        | other -> failwithf "received: %S" other ()
        end
      in
      loop (size - 1)
    | other ->
      say_item "As A Slave";
      Mpi.(send "aaa" 0 0 comm_world);
      say_item "Sent %S to `[0]` (tag 0)" "aaa";
    end;
    ()
  )
(*doc
Running it on 4 processes:

    mpirun -np 4 ./mpi_tests example01

gives:
*)
(*result "mpirun -np 4 ./mpi_tests example01" *)
(*doc

--------------------------------------------------------------------------------

The MPI Interface
=================

*)
(*result "echo '```' && echo 'module Mpi = Mpi' > /tmp/ompi.ml && ocamlfind ocamlc -i -package mpi -thread /tmp/ompi.ml && echo '```'" *)
