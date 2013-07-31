(*doc

Experimenting With String-Char Functors
=======================================


Compilation & run:

    ocamlfind ocamlc -package core,sexplib.syntax -linkpkg  -annot \
        -syntax camlp4o -thread src/exp/meta_parsing.ml -o meta_parsing \
       && ./meta_parsing ALL


To view this in HTML:

    caml2html -nf -charset UTF-8 src/exp/meta_parsing.ml \
      -ext "doc: pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"  \
      -ext "style: awk 'BEGIN {print \"<style>\"} { print } END {  printf \"</style>\" }'" \
      -ext "result: xargs meta_parsing | pandoc | awk  'BEGIN {print \"<div class=result_box>\"} { print } END {  printf \"</div>\" }'"

*)
(*style
body {max-width: 60em; margin: auto}
code, pre {background-color: #C9C7F1 }
.text_box { max-width: 50em; font-size: 90%;
  margin-left: 0em; border-left: 2px; font-family: serif }

.result_box { max-width: 47; font-size: 90%;background-color: #fee;
  margin-left: 3em; border-left: 2px; font-family: sans-serif }
.result_box code, .result_box pre {background-color: #fee;}

.text_box code, .text_box pre {background-color: #ddd;}
.text_box pre, .result_box pre {
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

A type `'a grammar`, and basic constructors, then functions:

- `parse: Sexp.t -> t grammar -> t`

or, some type `dsl format_t`, and functions:

- `parse: Sexp.t -> dsl format_t -> dsl`
- `print: dsl format_t -> dsl -> Sexp.t`


Implementation(s)
-----------------

Let's define the usual stuff:
*)
open Core.Std
let say fmt = printf ("    " ^^ fmt ^^ "\n%!")
let args = Array.to_list Sys.argv
let if_arg s f =
  if List.mem args "ALL" || List.mem args s then f ()

(*doc

### First Attempt

*)

let example_1 = "
    (dsl
     (let x 42)
     (let y (tuple 2 3))
     x
    )
"

type expr =
  | Dsl_let of string * expr
  | Dsl_int of int
  | Dsl_tuple of expr * expr
  | Dsl of expr list

let var l x = Dsl_let (l, x)
let int i = Dsl_int i
let tuple x y = Dsl_tuple (x, y)

(*doc

At least “parsing” …

*)
module Basic_grammar = struct

  type grammar =
    | Keyword_and_list of string * grammar
    | Try_best of grammar list
    | Integer
    | Identifier

  let dsl_grammar =
    let rec expression =
      Try_best [
        Keyword_and_list ("tuple", expression);
        Integer;
        Identifier;
      ] in
    Keyword_and_list ("dsl",
                      Try_best [
                        Keyword_and_list ("let", expression);
                        expression;
                      ])

  let parse_sexp ~grammar sexp =
    let open Sexp in
    let rec go grammar sexp =
      match sexp, grammar with
      | List (h :: t), Keyword_and_list (kwd, subgram) when Atom kwd = h ->
        say "Got kwd: %s" kwd;
        List.iter t ~f:(go subgram)
      | any, Try_best l ->
        List.iter l ~f:(fun gram ->
            begin try go gram any with e -> () end)
      | Atom a, Integer -> say "Int: %s" a
      | Atom a, Identifier -> say "Ident: %s" a
      | _ -> assert false
    in
    go grammar sexp

  let do_basic_test sexp () =
    Sexp.of_string (String.strip sexp) |> parse_sexp ~grammar:dsl_grammar

(*doc
#### Test
*)
  let () = if_arg "example_1" (do_basic_test example_1)
(*result example_1 *)

end

module Not_so_basic_grammmar = struct

  (*doc
Still matches

    (dsl
     (let x 42)
     (let y (tuple 2 3))
     x
    )

*)
  type expr = [
    | `int of int
    | `var of string
    | `tuple of expr * expr
  ] with sexp
  type dsl = [
    | `let_binding of string * expr
    | `expr of expr
  ] list
  with sexp

  type _ grammar =
    | Tagged: string * 'a grammar -> 'a grammar
    | Tuple: 'a grammar * 'b grammar -> ('a * 'b) grammar
    | Sequence: 'a grammar -> 'a list grammar
    (* | Keyword_and_list:  string * 'a grammar -> 'a list grammar *)
    | Try_in_order: 'a grammar list -> 'a grammar
    | Integer: int grammar
    | Identifier: string grammar
    | Apply: 'a grammar * ('a -> 'b) -> 'b grammar

  let dsl_grammar =
    let rec expr_grammar =
      Try_in_order [
        Apply (
          Tagged (
            "tuple", Tuple (expr_grammar, expr_grammar)
          ),
          fun (a, b) -> `tuple (a, b)
        );
        Apply (Integer, fun i -> `int i);
        Apply (Identifier, fun s -> `var s);
      ] in
    Apply (
      Tagged (
        "dsl", Sequence (
          Try_in_order [
            Apply (
              Tagged (
                "let", Tuple (Identifier, expr_grammar)
              ),
              fun (id, expr) -> `let_binding (id, expr));
            Apply (expr_grammar, fun e -> `expr e);
          ]
        )
      ),
      fun statements -> (statements: dsl)
    )

  let parse_sexp ~grammar sexp =
    let open Sexp in
    let rec go: type b. b grammar -> Sexp.t -> b =
      fun grammar sexp ->
        match sexp, grammar with
        | any, Apply (gram, f) ->
          f (go gram any)
        | List (h :: t), Tagged (kwd, subgram) when h = Atom kwd ->
          say "Got kwd: %s" kwd;
          go subgram (List t)
        | List l, Sequence subgram ->
          say "Sequence!";
          List.map l (go subgram)
        | any, Try_in_order subgrams ->
          let rec loop = function
          | [] -> assert false
          | h :: t ->
            try go h any with e -> loop t
          in
          loop subgrams
        | List (h :: t), Tuple (gramleft, gramright) ->
          let left = go gramleft h in
          let right = go gramright (List t) in
          (left, right)
        | Atom a, Identifier -> say "Ident: %s" a; a
        | Atom a, Integer -> say "Int: %s" a; Int.of_string a
          (* List.iter t ~f:(go subgram); *)
        | List [one], gram -> go gram one
        | any, _ ->
          say "%s fails" (Sexp.to_string_hum any);
          assert false
    in
    go grammar sexp


  let do_basic_test sexp () =
    Sexp.of_string (String.strip sexp)
    |> parse_sexp ~grammar:dsl_grammar
    |> sexp_of_dsl
    |> Sexp.to_string
    |> say "%s"

(*doc
#### Test
*)
  let () = if_arg "example_1_1" (do_basic_test example_1)
(*result example_1_1 *)

(*doc

That's really cool, but cannot use functions to construct the parsers:


```ocaml
let apply ~f t = Apply (t, f)

let dsl_grammar =
  let rec expr_grammar =
    Try_in_order [
      apply (Tagged ("tuple", Tuple (expr_grammar, expr_grammar)))
          ~f:(fun (a, b) -> `tuple (a, b));
      Apply (Integer, fun i -> `int i);
      Apply (Identifier, fun s -> `var s);
  ...
```

gives me:

     File "src/exp/meta_parsing.ml", line 182, characters 6-230:
     Error: This kind of expression is not allowed as right-hand side of `let rec'



*)



end
