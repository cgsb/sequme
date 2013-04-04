(*doc
# Testing DSLs with Sexplib

Compilation & run:

    ocamlfind ocamlc -package core,sexplib.syntax -linkpkg  -annot \
        -syntax camlp4o -thread src/exp/sexp_dsl.ml -o sexp_dsl \
       && ./sexp_dsl


To view this in HTML:

    caml2html -nf -charset UTF-8 src/exp/sexp_dsl.ml \
      -ext "doc: pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"  \
      -ext "style: awk 'BEGIN {print \"<style>\"} { print } END {  printf \"</style>\" }'"

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

open Core.Std
let say fmt = eprintf (fmt ^^ "\n%!")


type dsl_expr = [
  | `plus of dsl_expr list
  | `minus of dsl_expr * dsl_expr
  | `int of int
  | `equals of dsl_expr * dsl_expr
]
with sexp
let to_string e = sexp_of_dsl_expr e |> Sexp.to_string_hum

let () =
  say "%s" (`plus [ (`minus (`int 42, `int 43)); `int 355] |> to_string);
  ()
(*doc
The output with a simple `with sexp` has (of course) too many parentheses.

     (plus ((minus ((int 42) (int 43))) (int 355)))

## We Want A Lighter Syntax

*)


let example_input = "
  ;; Comments
  (gzip (level 3) (zlib-buffer-size 42_000))
  ;; or (gzip :level 3 :zlib-buffer-size 42_000)
  (transform
    (input some_file.fastq.gz)
    (bam))
  (transform
    (input (tagged some_file1.fasta some_file2.fasta (fasta-fastq-couple)))
    :filter (trim 5 bp at end)
    :compute (my_statistic (on_sequence ((quality (basepair 5)) / (4 + 42))))
    (fastq))
"


(*doc

We would like it parsed to something like:

     {gzip = Some (`gzip (3, `size 42_000));
      todo = [
        `transform { input = `guess "some_file.fastq.gz";
                     filters = [];
                     statistics = [];
                     output = `bam };
        `transform { input = `fasta_fastq ("some_file1.fasta", "some_file2.fasta");
                     filters = [ `trim (`at_end 5) ];
                     statistics = [
                       ("my_statistic",
                       `on_sequence (`div (`quality (`bp 5), `add (`int 4, `int 42))));
                     ];
                     output = `fastq; }
      ];
      some_other_param = None;}

## We Want Errors With Locations

[Sexp_intf.S.Annotated] is the module for parsing S-expressions
annotated with location information (`range` is a record with two
`pow`):

    type t = Pre_sexp.Annotated.t =
    | Atom of range * Type.t
    | List of range * t list * Type.t

[Sexp_intf.S.With_layout] has S-expressions annotated with relative
source positions *and comments*:

    type t =
    | Atom of pos * string * string option
    | List of pos * t_or_comment list * pos
    type t_or_comment =
    | Sexp of t
    | Comment of comment
    type comment =
    | Plain_comment of pos * string
    | Sexp_comment of pos * comment list * t


[Sexp_intf.S.Annotated]: https://ocaml.janestreet.com/ocaml-core/109.15.00/doc/sexplib/Sexp_intf.S.Annotated.html
[Sexp_intf.S.With_layout]: https://ocaml.janestreet.com/ocaml-core/109.15.00/doc/sexplib/Sexp_intf.S.With_layout.html


`With_layout` does not have an `of_string` function, but `Annotated`
does, and it pretty scary:

    # Sexp.Annotated.of_string "atom";;
    - : Core.Std.Sexp.Annotated.t =
    Core.Std.Sexp.Annotated.Atom
     ({Core.Std.Sexp.Annotated.start_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 0;
         Core.Std.Sexp.Annotated.offset = 0};
       Core.Std.Sexp.Annotated.end_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 4;
         Core.Std.Sexp.Annotated.offset = 0}},
     Sexplib.Type.Atom "atom")

    # Sexp.Annotated.of_string "()";;
    - : Core.Std.Sexp.Annotated.t =
    Core.Std.Sexp.Annotated.List
     ({Core.Std.Sexp.Annotated.start_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 0;
         Core.Std.Sexp.Annotated.offset = 0};
       Core.Std.Sexp.Annotated.end_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 1;
         Core.Std.Sexp.Annotated.offset = 1}},
     [], Sexplib.Type.List [])

    # Sexp.Annotated.of_string "(atom)";;
    - : Core.Std.Sexp.Annotated.t =
    Core.Std.Sexp.Annotated.List
     ({Core.Std.Sexp.Annotated.start_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 0;
         Core.Std.Sexp.Annotated.offset = 0};
       Core.Std.Sexp.Annotated.end_pos =
        {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 5;
         Core.Std.Sexp.Annotated.offset = 5}},
     [Core.Std.Sexp.Annotated.Atom
       ({Core.Std.Sexp.Annotated.start_pos =
          {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 1;
           Core.Std.Sexp.Annotated.offset = 1};
         Core.Std.Sexp.Annotated.end_pos =
          {Core.Std.Sexp.Annotated.line = 1; Core.Std.Sexp.Annotated.col = 5;
           Core.Std.Sexp.Annotated.offset = 5}},
       Sexplib.Type.Atom "atom")],
     Sexplib.Type.List [Sexplib.Type.Atom "atom"])


## We Want “Composability”

Something like:

    val parse_arith: sexp -> (`arith of expr, _) result
    val parse_gzip_config: sexp list -> (`gzip_config of gzip, _) result
    val parse_comparison: (sexp -> ('a, _) result) -> sexp -> ('a comp, _) result
    val parse_bool_expr: (sexp -> ('a, _) result) -> sexp -> ('a Blang.t, _) result
    ...
    let parse_my_language sexp =
      parse_list [
          "gzip", parse_gzip_config;
          "condition", parse_bool_expr (parse_comparison parse_arith);
      ]

## Let's try …

*)
open Result

let find_annotated_exn a t =
  (*tip Let's hope we make to call find_annotated_exn only where it
       makes sense … *)
  Option.value_exn ~message:"find_annotated_exn messed up"
    (Sexp.Annotated.find_sexp a t)

(*doc

### parse_arith

This a simple arithmetic language: *)
type arith_expr = [
  | `plus of arith_expr * arith_expr
  | `minus of arith_expr
  | `int of int
  | `var of string
]
with sexp

(*doc Use [Sexp_intf.S.Annotated] and its functions `get_sexp`,
   `get_range`, and `find_sexp` not to have to treat all the crazy cases
   of `Annotated.t`. *)
let parse_arith sexp =
  let open Sexp in
  let error range e =
    let open Annotated in
    (*tip `Annotated.range` is actually not `Sexpable` so for now we
       convert it to string. *)
    fail (`parse_arith (sprintf "L%dC%d—L%dC%d" range.start_pos.line
          range.start_pos.col range.end_pos.line range.end_pos.col, e)) in
  let classify_ident ~range ident =
    let valid_var v =
      String.for_all v (function
      | 'a' .. 'z' | '_' -> true
      | c -> false) in
    try return (`int (Int.of_string ident))
    with _ ->
      if valid_var ident
      then return (`var ident)
      else error range (`invalid_identifier ident)
  in
  (*tip The *20 lines* parsing function: *)
  let rec parse_descent annotated : (arith_expr, _) Result.t =
    match Annotated.get_sexp annotated, Annotated.get_range annotated with
    | Atom atom, range ->
      classify_ident ~range atom
      >>= fun e ->
      return e
    | List (Atom "+" :: first :: rest), _ ->
      List.fold rest ~init:(parse_descent (find_annotated_exn annotated first))
        ~f:(fun m sxp ->
          m >>= fun left ->
          parse_descent (find_annotated_exn annotated sxp)
          >>= fun right ->
          return (`plus (left, right)))
    | List [Atom "-"; one], _ ->
      parse_descent (find_annotated_exn annotated one)
      >>= fun arg ->
      return (`minus arg)
    | other, range ->
      error range (`unexpected_expression (Sexp.to_string_hum other))
  in
  parse_descent sexp
  >>= fun expr ->
  return (`arith expr)


let test_parse_arith example =
  begin match Sexp.Annotated.of_string example |> parse_arith with
  | Ok (`arith o) ->
    say "example:\n  %s\n  %s"
      example (sexp_of_arith_expr o |> Sexp.to_string_hum ~indent:4)
  | Error e ->
    say "example:\n  %s\nERROR: %s" example
      (e |>
       <:sexp_of<
         [> `parse_arith of
              string *
              [> `invalid_identifier of Core.Std.String.t
               | `unexpected_expression of string ] ]
       >> |> Sexp.to_string_hum)
  end

let () =
  test_parse_arith "(+ my_var 42 (- 1) (+ only_one))";
  test_parse_arith "(+ error:the_colon 42 (- 1) (+ only_one))";
  test_parse_arith "(+ error_the_missing_operand (-))";
  ()

(*doc
### Gzip

Now the GZip case. *)

type gzip = int * [`size of int | `factor of float ]
with sexp

(*doc The function take a list of S-Expression, as the caller should have
   discriminated on a keyword like `"gzip"` or `"gzip-output"`. *)
let parse_gzip ?(default_level=3) sexp_list : (gzip, _) Result.t =
  let open Sexp in
  let error range e =
    let open Annotated in
    fail (`parse_gzip (sprintf "L%dC%d—L%dC%d" range.start_pos.line
          range.start_pos.col range.end_pos.line range.end_pos.col, e)) in
  let level = ref default_level in
  let zlib_bs = ref (`size 4096) in
  let rec go_through annotated_list =
    match annotated_list with
    | [] -> return ()
    | tag :: value :: more when Annotated.get_sexp tag = Atom ":level" ->
      let range = Annotated.get_range value in
      begin match Annotated.get_sexp value with
      | Atom v ->
        begin try level := Int.of_string v; return ()
        with _ -> error range (`level_not_an_integer v)
        end
      | other -> error range (`unexpected_expression other)
      end
      >>= fun () ->
      go_through more
    | tag :: value :: more
      when Annotated.get_sexp tag = Atom ":zlib-buffer-size" ->
      let range = Annotated.get_range value in
      begin match Annotated.get_sexp value with
      | Atom v ->
        begin try zlib_bs := `size (Int.of_string v); return ()
        with _ -> error range (`zlib_bs_not_an_integer v)
        end
      | List [Atom "factor"; Atom v] ->
        begin try zlib_bs := `factor (Float.of_string v); return ()
        with _ -> error range (`zlib_bs_factor_not_an_integer v)
        end
      | other -> error range (`unexpected_expression other)
      end
      >>= fun () ->
      go_through more
    | wrong_tag :: anything_else ->
      let range = Annotated.get_range wrong_tag in
      error range (`unexpected_expression (Annotated.get_sexp wrong_tag))
  in
  go_through sexp_list
  >>= fun () ->
  return (!level, !zlib_bs)


let test_parse_gzip example =
  let open Sexp in
  let to_parse = Sexp.Annotated.of_string example in
  begin match Sexp.Annotated.get_sexp to_parse with
  | List (Atom "gzip" :: rest) ->
    let sexps = List.map rest (find_annotated_exn to_parse) in
    begin match parse_gzip sexps with
    | Ok o ->
      say "example:\n  %s\n  %s"
        example (sexp_of_gzip o |> Sexp.to_string_hum ~indent:4)
    | Error e ->
      say "example:\n  %s\nERROR: %s" example
        (e |>
         <:sexp_of<
           [> `parse_gzip of
              string *
         [> `level_not_an_integer of string
          | `unexpected_expression of Core.Std.Sexp.t
          | `zlib_bs_factor_not_an_integer of string
          | `zlib_bs_not_an_integer of string ]
           ]
         >> |> Sexp.to_string_hum)
    end
  |  _ -> failwithf "wrongly designed test" ()
  end

let () =
  test_parse_gzip "(gzip)";
  test_parse_gzip "(gzip :level 42)";
  test_parse_gzip "(gzip :level 42 :zlib-buffer-size 40_232)";
  test_parse_gzip "(gzip :level 42 :zlib-buffer-size (factor 3.14))";
  test_parse_gzip "(gzip :level 42 :zlib-buffer-size 40_232 :level 5)";
  test_parse_gzip "(gzip :level_error 42)";
  test_parse_gzip "(gzip :level 42\n;; some comment\n:zlib-buffer-size (factor   ))";
  ()

(*doc
### Comparisons

This time they will be **infix**.
*)

type 'a comparison = [
  | `eq of 'a * 'a
  | `ne of 'a * 'a
] with sexp

(*doc Again, a list of s-expressions because the caller may have
   delimited the comparison itself.

- `parse_alpha` is the parsing of the `'a` in `'a comparison`

*)
let parse_comparison parse_alpha init_range sexp_list =
  let open Sexp in
  let error range e =
    let open Annotated in
    fail (`parse_comparison (sprintf "L%dC%d—L%dC%d" range.start_pos.line
          range.start_pos.col range.end_pos.line range.end_pos.col, e)) in
  begin match sexp_list with
  | left :: middle :: right :: [] ->
    parse_alpha left
    >>= fun a_left ->
    parse_alpha right
    >>= fun a_right ->
    let range = Annotated.get_range middle in
    begin match Annotated.get_sexp middle with
    | Atom "=" -> return (`eq (a_left, a_right))
    | Atom "<>" -> return (`ne (a_left, a_right))
    | other -> error range (`unexpected_expression other)
    end
  | some_list ->
    error init_range (`expecting_3_items (List.map some_list Annotated.get_sexp))
  end

let test_parse_comparison example =
  let open Sexp in
  let to_parse = Sexp.Annotated.of_string (sprintf "(comp: %s)" example) in
  begin match Sexp.Annotated.get_sexp to_parse with
  | List (Atom "comp:" :: rest) ->
    let sexps = List.map rest (find_annotated_exn to_parse) in
    let unboxed_parse_arith s =
      parse_arith s
      >>= fun (`arith a) ->
      return a in
    let range = Annotated.get_range to_parse in
    begin match parse_comparison unboxed_parse_arith range sexps with
    | Ok o ->
      say "example:\n  %s\n  %s"
        example (sexp_of_comparison sexp_of_arith_expr o |> Sexp.to_string_hum ~indent:4)
    | Error e ->
      say "example:\n  %s\nERROR: %s" example
        (e |>
         <:sexp_of<
         [> `parse_arith of
              string *
              [> `invalid_identifier of Core.Std.String.t
               | `unexpected_expression of string ]
          | `parse_comparison of
              string *
              [> `expecting_3_items of Core.Std.Sexp.t Core.Std.List.t
               | `unexpected_expression of Core.Std.Sexp.t ] ]
         >> |> Sexp.to_string_hum)
    end
  |  _ -> failwithf "wrongly designed test" ()
  end

let () =
  test_parse_comparison  "42 = 43";
  test_parse_comparison  "42 = (+ 21 21)";
  test_parse_comparison  "42error = (+ 21 21)";
  test_parse_comparison  "= (+ 21 21)";
  say ""

(*doc

### Boolean Expressions With Core's Blang

Here the trick would be to leverage Core's already-written Blang parser.
But it does *not* use `Sexp.Annotated.t` so can not completely implement it.

So this is just a *type-checking proof-of-concept*:
*)

let parse_boolean_expression
    (type err) (parse_atom : Sexp.t -> ('a, err) Result.t) sexp : ('a Blang.t, _) Result.t =
  let module With_exceptions = struct
    exception Parse_atom of err
    let atom_of_sexp s =
      match parse_atom s with
      | Ok a -> a
      | Error e -> raise (Parse_atom e)

    let parse_blang sexp =
      try
        Ok (Blang.t_of_sexp atom_of_sexp sexp)
      with
      | Parse_atom e -> fail (`parse_blang_atom e)
      | e -> fail (`parse_blang e)

  end in
  With_exceptions.parse_blang sexp

(*doc

### Parse / Discriminate A List


*)

let parse_list assoc sexp_list =
  let open Sexp in
  let error range e =
    let open Annotated in
      fail (`parse_list (sprintf "L%dC%d—L%dC%d" range.start_pos.line
          range.start_pos.col range.end_pos.line range.end_pos.col, e)) in
  List.fold sexp_list ~init:(return []) ~f:(fun m sexp ->
    m >>= fun prev ->
    let range = Annotated.get_range sexp in
    begin match Sexp.Annotated.get_sexp sexp with
    | List (Atom atom :: rest) ->
      begin match List.Assoc.find assoc atom with
      | Some parse_atom ->
        let sexps = List.map rest (find_annotated_exn sexp) in
        parse_atom sexps
        >>= fun v ->
        return (v :: prev)
      | None ->
        error range (`tag_not_found atom)
      end
    | other -> error range (`unexpected_expression other)
    end)

(*doc

The interesting part of the test is the build of the config-file parser:


    let my_config_file =
      parse_list [
        ("gzip", (fun sexps -> parse_gzip ~default_level:4 sexps >>= fun r -> return (`gzip r)));
        ("compare", (fun sexps ->
           parse_comparison parse_arith init_range sexps >>= fun r -> return (`compare r)));
      ] in

*)
let test_parse_list  example =
  let to_parse = Sexp.Annotated.of_string (sprintf "(%s)" example) in
  let init_range = Sexp.Annotated.get_range to_parse in
  let my_config_file =
    parse_list [
      ("gzip", (fun sexps -> parse_gzip ~default_level:4 sexps >>= fun r -> return (`gzip r)));
      ("compare", (fun sexps ->
         parse_comparison parse_arith init_range sexps >>= fun r -> return (`compare r)));
    ] in
  let open Sexp in
  match Annotated.get_sexp to_parse with
  | List l ->
    begin match my_config_file (List.map ~f:(find_annotated_exn to_parse) l) with
    | Ok v ->
      let s = <:sexp_of<
         [> `compare of [> `arith of arith_expr ] comparison
          | `gzip of gzip ] list
        >> v |> Sexp.to_string_hum in
      say "example:\n  %s\n  %s" example s
    | Error e ->
      let s = <:sexp_of<
         [> `parse_arith of
              string *
              [> `invalid_identifier of Core.Std.String.t
               | `unexpected_expression of string ]
          | `parse_comparison of
              string *
              [> `expecting_3_items of Sexp.t Core.Std.List.t
               | `unexpected_expression of Core.Std.Sexp.t ]
          | `parse_gzip of
              string *
              [> `level_not_an_integer of string
               | `unexpected_expression of Core.Std.Sexp.t
               | `zlib_bs_factor_not_an_integer of string
               | `zlib_bs_not_an_integer of string ]
          | `parse_list of
              string *
              [> `tag_not_found of string
               | `unexpected_expression of Core.Std.Sexp.t ] ]
              >> e |> Sexp.to_string_hum in
      say "example:\n  %s\nERROR: %s" example s
    end
  | Atom e ->
    say "ERROR ATOM in test_parse_list"

let () =
  test_parse_list "(gzip)";
  test_parse_list "(gzip :level 4) (compare (+ 21 21) = 42)";
  test_parse_list "(gzip :zlib-buffer-size (factor 3434e)) (compare (+ 21 21) = 42)";
  test_parse_list "(gzip :level 9) (compare (+ 21 2.1) = 42)";
  say ""
(*doc

This gives:

    example:
      (gzip)
      ((gzip (4 (size 4096))))
    example:
      (gzip :level 4) (compare (+ 21 21) = 42)
      ((compare (eq ((arith (plus ((int 21) (int 21)))) (arith (int 42)))))
     (gzip (4 (size 4096))))
    example:
      (gzip :zlib-buffer-size (factor 3434e)) (compare (+ 21 21) = 42)
    ERROR: (parse_gzip (L1C25—L1C38 (zlib_bs_factor_not_an_integer 3434e)))
    example:
      (gzip :level 9) (compare (+ 21 2.1) = 42)
    ERROR: (parse_arith (L1C32—L1C35 (invalid_identifier 2.1)))

*)
