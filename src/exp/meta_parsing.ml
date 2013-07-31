(*doc

GADTs-based Grammars
====================


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


Experimentations
----------------

Let's define the usual stuff:
*)
open Core.Std
let indentation = ref 4
let say fmt = ksprintf (fun s ->
    let sep = (String.make !indentation ' ') in
    printf "%s%s\n%!" sep (String.split ~on:'\n' s |> String.concat ~sep:("\n" ^ sep))
  ) fmt
let args = Array.to_list Sys.argv
let if_arg s f =
  if List.mem args "ALL" || List.mem args s then f ()

(*doc

What we are going to try to parse:

*)
let example_1 = "
    (dsl
     (let x 42)
     (let y (tuple 2 3))
     x)
"
(*doc
### First Attempt: At least “parsing” …

In `Basic_grammar`, we just go through the S-Expression and print stuff.
*)
module Basic_grammar = struct

  type grammar =
    | Keyword_and_list of string * grammar
    | Try_best of grammar list
    | Integer
    | Identifier

(*doc

Note: in this experiment, all the parsers use `assert false` to fail.
*)
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

(*doc

and that's how we define the grammar for the DSL in `example_1`:

*)
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
(*doc
Testing:
*)
  let do_basic_test sexp () =
    Sexp.of_string (String.strip sexp) |> parse_sexp ~grammar:dsl_grammar

  let () = if_arg "example_1" (do_basic_test example_1)
(*result example_1 *)

end

(*doc

### Second Attempt: A GADT

In `Not_so_basic_grammmar` we use a GADT to implement the desired
`parse` signature.

*)
module Not_so_basic_grammmar = struct

  type _ grammar =
    | Tagged: string * 'a grammar -> 'a grammar
    | Tuple: 'a grammar * 'b grammar -> ('a * 'b) grammar
    | Sequence: 'a grammar -> 'a list grammar
    | Try_in_order: 'a grammar list -> 'a grammar
    | Integer: int grammar
    | Identifier: string grammar
    | Apply: 'a grammar * ('a -> 'b) -> 'b grammar

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

  (*doc
Let's define an AST that matches `example_1`:

    (dsl
     (let x 42)
     (let y (tuple 2 3))
     x)

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
(*doc

And we express the grammar with the GADT:
*)
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


(*doc
and the test:
*)
  let do_basic_test sexp  grammar () =
    Sexp.of_string (String.strip sexp)
    |> parse_sexp ~grammar
    |> sexp_of_dsl
    |> Sexp.to_string
    |> say "%s"
  let () = if_arg "example_1_1" (do_basic_test example_1 dsl_grammar)
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

Let's make it with functions:
*)
  let apply g ~f = fun () -> (Apply (g (), f))
  let sequence lg = fun () -> Sequence (lg ())
  let try_in_order l = fun () -> Try_in_order (List.map l ~f:(fun g -> g ()))
  let tagged lg ~tag = fun () -> Tagged (tag, lg ())
  let tuple x y = fun () -> Tuple (x (), y ())
  let integer = fun () -> Integer
  let identifier = fun () -> Identifier
(*doc
This is an infinite loop, it raises `Stack_overflow`
(without the `()`, i.e. `let dsl_grammar =`):
 *)
  let dsl_grammar () =
    let rec expr_grammar () =
      try_in_order [
        apply (tagged ~tag:"tuple"
                 (tuple expr_grammar expr_grammar))
          ~f:(fun (a, b) -> `tuple (a, b));
        apply integer ~f:(fun i -> `int i);
        apply identifier ~f:(fun s -> `var s);
      ] () in
    apply ~f:(fun statements -> (statements: dsl))
      (tagged ~tag:"dsl"
         (sequence
            (try_in_order [
                apply  ~f:(fun (id, expr) -> `let_binding (id, expr))
                  (tagged ~tag:"let" (tuple identifier expr_grammar));
                apply ~f:(fun e -> `expr e) expr_grammar;
              ])))
      ()
(*doc

So, let's make it `lazy`:
*)
  let apply g ~f = lazy (Apply (Lazy.force g, f))
  let sequence lg = lazy (Sequence (Lazy.force lg))
  let try_in_order l = lazy (Try_in_order (List.map l ~f:(fun g -> Lazy.force g)))
  let tagged lg ~tag = lazy (Tagged (tag, Lazy.force lg))
  let tuple x y = lazy (Tuple (Lazy.force x, Lazy.force y))
  let integer = lazy Integer
  let identifier = lazy Identifier
(*doc

This raises `CamlinternalLazy.Undefined`:
*)
  let dsl_grammar () =
    let rec expr_grammar =
      lazy (
      try_in_order [
        apply (tagged ~tag:"tuple"
                 (tuple (Lazy.force expr_grammar) (Lazy.force expr_grammar)))
          ~f:(fun (a, b) -> `tuple (a, b));
        apply integer ~f:(fun i -> say "Int!: %d" i; `int i);
        apply identifier ~f:(fun s -> `var s);
      ] ) in
    apply ~f:(fun statements -> (statements: dsl))
      (tagged ~tag:"dsl"
         (sequence
            (try_in_order [
                apply  ~f:(fun (id, expr) -> `let_binding (id, expr))
                  (tagged ~tag:"let" (tuple identifier (Lazy.force expr_grammar)));
                apply ~f:(fun e -> `expr e) (Lazy.force expr_grammar);
              ])))
    |> Lazy.force

end

(*doc

### Third Attempt: A *Lazy* GADT

Let's put the *lazyness* in the GADT itself:

*)
module More_complex_grammar = struct

  type _ grammar =
    | Tagged: string * 'a grammar Lazy.t -> 'a grammar
    | Tuple: 'a grammar Lazy.t * 'b grammar Lazy.t -> ('a * 'b) grammar
    | Sequence: 'a grammar Lazy.t -> 'a list grammar
    (* | Keyword_and_list:  string * 'a grammar -> 'a list grammar *)
    | Try_in_order: 'a grammar Lazy.t list -> 'a grammar
    | Integer: int grammar
    | Identifier: string grammar
    | Apply: 'a grammar Lazy.t * ('a -> 'b) -> 'b grammar

  let apply g ~f = lazy (Apply (g, f))
  let sequence lg = lazy (Sequence (lg))
  let try_in_order l = lazy (Try_in_order l)
  let tagged lg ~tag = lazy (Tagged (tag, lg))
  let tuple x y = lazy (Tuple (x, y))
  let integer = lazy Integer
  let identifier = lazy Identifier

  (*doc
The same `parse_sexp` function but with some `lazy` keywords and
`Lazy.force` calls:

  *)
  let parse_sexp ~grammar sexp =
    let open Sexp in
    let rec go: type b. b grammar Lazy.t -> Sexp.t -> b =
      fun grammar sexp ->
        match sexp, Lazy.force grammar with
        | any, Apply (gram, f) ->
          f (go gram any)
        | List (h :: t), Tagged (kwd, subgram) when h = Atom kwd ->
          (* say "Got kwd: %s" kwd; *)
          go subgram (List t)
        | List l, Sequence subgram ->
          (* say "Sequence!"; *)
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
        | List [one], gram -> go (lazy gram) one
        | any, _ ->
          say "%s fails" (Sexp.to_string_hum any);
          assert false
    in
    go grammar sexp

(*doc

This is the same AST as before:

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

  (*doc
To make OCaml recursive definitions checker happy, we need to protect them:

```ocaml
let rec my_grammar = lazy (Lazy.force ( (* ... use my_grammar ... *) ))
```

If we define

```ocaml
let protect_rec m = lazy (Lazy.force m)
```

we still get the forbidden recursive definition because it requires
the explicit `lazy` keyword (c.f. [the manual][ocaml/manual/letrecdef]).

[ocaml/manual/letrecdef]: http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#toc70
  *)
  let dsl_grammar =
    let rec expr_grammar =
      lazy (Lazy.force (
        try_in_order [
          apply (tagged ~tag:"tuple"
                   (tuple (expr_grammar) (expr_grammar)))
            ~f:(fun (a, b) -> `tuple (a, b));
          apply integer ~f:(fun i -> say "Int!: %d" i; `int i);
          apply identifier ~f:(fun s -> `var s);
        ]
      )) in
    apply ~f:(fun statements -> (statements: dsl))
      (tagged ~tag:"dsl"
         (sequence
            (try_in_order [
                apply  ~f:(fun (id, expr) -> `let_binding (id, expr))
                  (tagged ~tag:"let" (tuple identifier (expr_grammar)));
                apply ~f:(fun e -> `expr e) (expr_grammar);
              ])))

(*doc
and the test:
*)
  let do_basic_test sexp  grammar () =
    Sexp.of_string (String.strip sexp)
    |> parse_sexp ~grammar
    |> sexp_of_dsl
    |> Sexp.to_string
    |> say "%s"
  let () = if_arg "example_1_4" (do_basic_test example_1 dsl_grammar)
(*result example_1_4 *)

end

(*doc

A Better API, A Better Parsing
------------------------------

*)

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]
let return x : (_, _) result = `Ok x
let fail x : (_, _) result = `Error x
let (>>=) x f : (_, _) result =
  match x with
  | `Ok o -> f o
  | `Error e -> `Error e
let (>><) x f : (_, _) result = f x

module type META_PARSER = sig

  type ('dsl, 'error) grammar
  type ('dsl, 'error) t = ('dsl, 'error) grammar lazy_t
  (** Base grammar holder *)

  (** {3 Basic Constructors } *)

  val keyword : string -> (unit, 'error) t
  val left_and_continue :
    ('a, 'b) t -> ('c, 'b) t -> ('a * 'c, 'b) t
  val sequence: ('a, 'b) t -> ('a list, 'b) t
  val try_in_order: ('a, 'b) t list -> ('a, 'b) t

  val integer : (int, 'a) t
  val string :  (string, 'a) t
  val float :  (float, 'a) t
  val apply :
    ('a, 'error) t -> f:('a -> ('c, 'error) result) -> ('c, 'error) t

  (** {3 High-level Constructors } *)

  val tagged : ('a, 'b) t -> tag:string -> ('a, 'b) t
  val tuple:
    ('a, 'b) t -> ('c, 'b) t -> ('a * 'c, 'b) t

  (** {3 Parsing function } *)

  type syntax_error =
    [ `not_a_float of string
    | `not_an_integer of string
    | `no_matching_rule of string
    | `nothing_left_to_try of string ]
  with sexp

  val parse:
    syntax_error:(string -> [> syntax_error] -> 'error) ->
    ('dsl, 'error) t -> Sexp.Annotated.t -> ('dsl, 'error) result

end

module Meta_parser : META_PARSER = struct

  type (_, 'error) grammar =
    | Keyword: string -> (unit, 'error) grammar
    | Left_and_continue:
        ('a, 'error) grammar Lazy.t * ('b, 'error) grammar Lazy.t -> ('a * 'b, 'error) grammar
    | Sequence: ('a, 'error) grammar Lazy.t -> ('a list, 'error) grammar
    | Try_in_order: ('a, 'error) grammar Lazy.t list -> ('a, 'error) grammar
    | Integer: (int, 'error) grammar
    | Float: (float, 'error) grammar
    | String: (string, 'error) grammar
    | Apply: ('a, 'error) grammar Lazy.t * ('a -> ('b, 'error) result) -> ('b, 'error) grammar

  type ('dsl, 'error) t = ('dsl, 'error) grammar Lazy.t


  let rec to_string_aux: type a. int -> (a, _) grammar -> string  =
    fun level  gram ->
      if level < 0 then "..."
      else
        let to_string: type a. (a, _) grammar -> string =
          fun x -> to_string_aux (level - 1) x in
        match gram with
        | Keyword s -> sprintf "Kwd %s" s
        | Left_and_continue (lg, rg) ->
          sprintf "parse [%s] and continue with [%s]"
            (Lazy.force lg |> to_string) (Lazy.force rg |> to_string)
        | Sequence g -> sprintf "sequence [%s]" (Lazy.force g |> to_string)
        | Try_in_order gl ->
          sprintf "try %s"
            (List.map gl (fun g -> Lazy.force g |> to_string |> sprintf "[%s]")
             |> String.concat ~sep:" then ")
        | Integer -> "int"
        | Float -> "float"
        | String -> "string"
        | Apply (lg, f) -> sprintf "{%s}" (Lazy.force lg |> to_string)

  let to_string ?(max_level=3) t = (Lazy.force t |> to_string_aux max_level)


  let keyword s = lazy (Keyword s)
  let left_and_continue left continue = lazy (Left_and_continue (left, continue))
  let sequence lg = lazy (Sequence (lg))
  let try_in_order l = lazy (Try_in_order l)

  let integer = lazy Integer
  (* let identifier = lazy Identifier; *)
  let string = lazy String
  let float = lazy Float

  let apply g ~f = lazy (Apply (g, f))

  let tagged lg ~tag =
    apply
      (left_and_continue (keyword tag) lg)
      (fun ((), c) -> return c)

  let tuple x y = left_and_continue x y


  let find_annotated_exn a t =
    (*tip Let's hope we are going to call find_annotated_exn only where it
        makes sense … *)
    match (Sexp.Annotated.find_sexp a t) with
    | Some s -> s
    | None ->
      failwithf "find_annotated_exn: %s In %s"
        (Sexp.to_string_hum t) (Sexp.Annotated.get_sexp a |> Sexp.to_string_hum) ()

  let remake_annotated_from_list_exn range annotated t =
    let open Sexp in
    (Annotated.List (range, List.map t (find_annotated_exn annotated), List t))

  type syntax_error =
    [ `not_a_float of string
    | `not_an_integer of string
    | `no_matching_rule of string
    | `nothing_left_to_try of string ]
  with sexp


  let parse ~syntax_error grammar sexp =
    let gram_to_string = to_string in
    let parsing_error range e =
      let open Sexp.Annotated in
      let loc =
        sprintf "L%dC%d—L%dC%d" range.start_pos.line
          range.start_pos.col range.end_pos.line range.end_pos.col in
      fail (syntax_error loc e)
    in
    let open Sexp in
    let rec go: type b. (b, 'e) grammar Lazy.t -> Sexp.Annotated.t -> (b, 'e) result =
      fun grammar annotated ->
        let sexp = Annotated.get_sexp annotated in
        let range = Annotated.get_range annotated in
        match sexp, Lazy.force grammar with
        | any, Apply (gram, f) ->
          go gram annotated
          >>= fun r ->
          f r
        | Atom a, Keyword k when Atom k = Atom a ->
          (* say "Kwd: %s" a; return () *)
          return ()
        | List (h :: hh :: t), Left_and_continue (gramleft, gramright) ->
          (* say "go %s Vs %s" (Sexp.to_string_hum sexp) (gram_to_string grammar); *)
          go gramleft (find_annotated_exn annotated h)
          >>= fun left ->
          go gramright (remake_annotated_from_list_exn range annotated (hh :: t))
          >>= fun right ->
          return (left, right)
        | List l, Sequence subgram ->
          List.fold l ~init:(return []) ~f:(fun prev_m sexp ->
              prev_m >>= fun l ->
              go subgram (find_annotated_exn annotated sexp)
              >>= fun r ->
              return (l @ [r]))
        | any, Try_in_order subgrams  ->
          let rec loop = function
          | [] ->
            parsing_error range (`nothing_left_to_try
                                   (gram_to_string ~max_level:4 grammar))
          | h :: t ->
            match go h annotated with
            | `Ok o -> return o
            | `Error e -> loop t
          in
          loop subgrams
        | Atom a, String -> (* say "Str: %s" a;  *)return a
        | Atom a, Integer -> (* say "Int: %s" a; *)
          (try return (Int.of_string a) with
           | e -> parsing_error range (`not_an_integer a))
        | Atom a, Float -> (* say "Int: %s" a; *)
          (try return (Float.of_string a) with
           | e -> parsing_error range (`not_a_float a))
        | List [one], gram -> go (lazy gram) (find_annotated_exn annotated one)
        | any, _ ->
          parsing_error range (`no_matching_rule (Sexp.to_string any))
    in
    go grammar sexp

end

module Test_meta_parser = struct
  open Meta_parser
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

  let dsl_grammar () =
    let identifier =
      apply string (fun s ->
          if String.for_all s Char.is_alphanum
          then return s else fail (`not_an_identifier s))
    in
    let rec expr_grammar =
      lazy (Lazy.force (
          try_in_order [
            apply (tagged ~tag:"tuple"
                     (tuple (expr_grammar) (expr_grammar)))
              ~f:(fun (a, b) -> return (`tuple (a, b)));
            apply integer ~f:(fun i -> return (`int i));
            apply identifier ~f:(fun s -> return (`var s));
          ]
        )) in
    apply ~f:(fun statements -> return (statements: dsl))
      (tagged ~tag:"dsl"
         (sequence
            (try_in_order [
                apply  ~f:(fun (id, expr) -> return (`let_binding (id, expr)))
                  (tagged ~tag:"let" (tuple identifier (expr_grammar)));
                apply ~f:(fun e -> return (`expr e)) (expr_grammar);
              ])))

  (*doc
    and the tests:
  *)
  let do_basic_test sexp  grammar () =
    let syntax_error loc e = `syntax_error (loc, e) in
    match (Sexp.Annotated.of_string (String.strip sexp) |> parse ~syntax_error grammar) with
    | `Ok dsl ->
      dsl |> sexp_of_dsl |> Sexp.to_string_hum |> say "%s"
    | `Error e ->
      say "Error: %s"
        (<:sexp_of<
           [> `syntax_error of string * [> Meta_parser.syntax_error ]
           | `not_an_identifier of string]
         >>  e |> Sexp.to_string_hum);
      ()
  let () =
    if_arg "example_2" (do_basic_test "
    (dsl
     (let (x )(42)) ;; some comment
     (let y ((tuple 2 3)))
     (tuple x (tuple y 42))
     x)
" (dsl_grammar ()))
    (*result example_2 *)

  let () =
    if_arg "example_3" (do_basic_test "
    (dsl
     (let (x )(42)) ;; some comment
     (let y ((tuple 2 3)))
     (tuple x (tuple y 42 error))
     x)
" (dsl_grammar ()))
    (*result example_3 *)

  let () =
    if_arg "example_4" (do_basic_test "
    (dsl
     (let (x )(42)) ;; some comment
     (leterror y ((tuple 2 3)))
     (tuple x (tuple y 42))
     x)
" (dsl_grammar ()))
    (*result example_4 *)
end
