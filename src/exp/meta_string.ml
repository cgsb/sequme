
(*doc

 Experimenting With String-Char Functors
=======================================


Compilation & run:

    ocamlfind ocamlc -package core,sexplib.syntax -linkpkg  -annot \
        -syntax camlp4o -thread src/exp/meta_string.ml -o meta_string \
       && ./meta_string ALL


To view this in HTML:

    caml2html -nf -charset UTF-8 src/exp/meta_string.ml \
      -ext "doc: pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"  \
      -ext "style: awk 'BEGIN {print \"<style>\"} { print } END {  printf \"</style>\" }'" \
      -ext "result: xargs meta_string | pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"

*)
(*style
body {max-width: 60em; margin: auto}
code, pre {background-color: #C9C7F1 }
.text_box { max-width: 50em; font-size: 90%;
  margin-left: 0em; border-left: 2px; font-family: sans-serif }
.text_box code, .text_box pre {background-color: #ddd;}
.text_box pre {
  margin-left: 2em; margin-right: 2em;
  white-space: pre-wrap;
  padding-left: 1em;
  text-indent: 0em;
}
.text_tip p { display: inline; }
*)
(*doc

Requirements
------------

Char, String modules should be any pair:

- OCaml `string`
- [Rope]
- Bigarray (→ compatible only with `int` or `char` *chars* …)
- OCaml `char`
- UTF8 char
- limited alphabet char (e.g. `A,C,G,T,N`) with potential use of less
  than 8 bits
- variable encoding char (Huffman tree?)

[Rope]: http://rope.forge.ocamlcore.org/

First Experiments
-----------------

We could try to implement the same interface with:

- OCaml `string` and `char`
- `non-balanced ropes` of `int` values
- bigarrays of 4-bits SAM-chars

Let's define the usual stuff:
*)
open Core.Std
let say fmt = printf (fmt ^^ "\n%!")
let args = Array.to_list Sys.argv
let if_arg s f =
  if List.mem args "ALL" || List.mem args s then f ()

(*doc

Minimal API
-----------

*)
module type CHAR = sig

  type t
  type string

  val serialize: t -> string -> int -> (int, [> `out_of_bounds ]) Result.t
  (** [serialize t s pos] writes [t] in [s] at position [pos] and
      return the amount of bits used. *)

  val unserialize: string -> int -> ((t * int), [> `out_of_bounds]) Result.t

  val to_string_hum: t -> String.t

end
module type STRING = sig

  type t
  type char

  val of_char: char -> t
  val of_char_list: char list -> t

  val get: t -> int -> char
  (** Get the n-th char, not necessarily bytes or bits. *)

  val set: t -> int -> char -> t
  (** String should not be mutable. *)

  val concat: t list -> ?sep:t -> t

end
