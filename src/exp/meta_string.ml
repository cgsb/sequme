
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
  margin-left: 0em; border-left: 2px; font-family: serif }
.text_box code, .text_box pre {background-color: #ddd;}
.text_box pre {
  margin-left: 2em; margin-right: 2em;
  white-space: pre-wrap;
  padding-left: 1em;
  text-indent: 0em;
}
blockquote {background-color: #eee}
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

Mutually Recursive Functor Applications
---------------------------------------

We *might* need them (?).

Recursive modules are a “language extension”, c.f. [the manual
7.8][ocaml-man-78]:

> This is an experimental extension of OCaml: the class of recursive
> definitions accepted, as well as its dynamic semantics are not final
> and subject to change in future releases.

[ocaml-man-87]: http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#toc75

Just checking how far we can go:
*)
module type A = sig
  type t
  type ext
  val t: ext -> t
end
module type B = sig
  type t
  type ext
  val u: t -> ext -> unit
end

module A_one (B_impl : B) : A = struct

  type t = int
  type ext = B_impl.t
  let t ext = 42

end
module B_one (A_impl : A) : B = struct

  type t = string
  type ext = A_impl.t
  let u t e = ()

end

(*doc This works: *)
module rec Aa : A  = A_one(Bb) and Bb : B = B_one(Aa)

(*doc
This does not:

    module rec Aaa : A with type ext := Bb.t = A_one(Bbb)
           and Bbb : B with type ext := Aaa.t = B_one(Aaa)


    File "src/exp/meta_string.ml", line 160, characters 53-56:
    Error: Signature mismatch:
           Modules do not match:
             sig type t = Bbb.t val u : t -> Aaa.t -> unit end
           is not included in
             B
           The field `ext' is required but not provided

and neither do this:

    module rec Aaa : A with type ext = Bbb.t = A_one(Bbb)
           and Bbb : B with type ext = Aaa.t = B_one(Aaa)

or this

    module rec Aaa : A with type ext = string = A_one(Bbb)
           and Bbb : B with type ext = int = B_one(Aaa)


Minimal API
-----------

The independent `CHAR` and `STRING` signatures would require recursive
functors/modules:

```ocaml
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
```

Let's put a `Char` module in the `String` module:

Also, let's replace:

    val serialize: t -> string -> int -> (int, [> `out_of_bounds ]) Result.t
    (** [serialize t s pos] writes [t] in [s] at position [pos] and
        return the amount of bits used. *)

with an immutable string version.
*)


module type STRING = sig

  type t

  module Char: sig
    type string = t
    type t


    val serialize: t -> (string * int)

    val unserialize: string -> int -> ((t * int), [> `out_of_bounds]) Result.t

    val to_string_hum: t -> String.t

  end

  val of_char: Char.t -> t
  val of_char_list: Char.t list -> t

  val get: t -> int -> Char.t option
  (** Get the n-th char, not necessarily bytes or bits. *)

  val set: t -> int -> Char.t -> t option
  (** String should not be mutable. *)

  val concat: ?sep:t -> t list -> t

end

(*doc

### Ocaml Strings

*)
module type BASIC_OCAML_LIKE_STRING = sig

  type t
  val get: t -> int -> char
  val of_char: char -> t

end


module OCaml_char (S: BASIC_OCAML_LIKE_STRING) = struct

  type string = S.t
  type t = char
  let serialize c = (S.of_char c, 8)
  let unserialize s p =
    try Ok (S.get s p, 8)
    with e -> Error (`out_of_bounds)

  let to_string_hum c = S.of_char c
end

module OCaml_string : STRING with type t = string = struct

  module Basic_string: BASIC_OCAML_LIKE_STRING with type t = string = struct
    type t = string
    let get = String.get
    let of_char = String.make 1
  end

  include Basic_string

  module Char = OCaml_char(Basic_string)

  let get s p =
    try Some (String.get s p)
    with e -> None

  let concat ?(sep="") t = String.concat t ~sep

  let set str pos c =
    try
      let new_one = String.copy str in
      new_one.[pos] <- c;
      Some new_one
    with e -> None

  let of_char_list cl = concat ?sep:None (List.map cl ~f:of_char)

end



(*doc

The Main Test
-------------
*)
let () =
  say "Go!";
  ()
