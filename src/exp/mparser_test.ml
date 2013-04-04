
(*doc
# Testing DSLs with MParser

Compilation & run:

    ocamlfind ocamlc -package core,sexplib.syntax,mparser -linkpkg  -annot \
        -syntax camlp4o -thread src/exp/mparser_test.ml -o mparser_test \
       && ./mparser_test ALL


To view this in HTML:

    caml2html -nf -charset UTF-8 src/exp/mparser_test.ml \
      -ext "doc: pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"  \
      -ext "style: awk 'BEGIN {print \"<style>\"} { print } END {  printf \"</style>\" }'" \
      -ext "result: xargs mparser_test | pandoc | awk  'BEGIN {print \"<div class=text_box>\"} { print } END {  printf \"</div>\" }'"

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
let say fmt = printf (fmt ^^ "\n%!")
let args = Array.to_list Sys.argv
let if_arg s f =
  if List.mem args "ALL" || List.mem args s then f ()

(*doc

The documentation in [mparser.mli] and [charStream.mli] is quite
complete and the library only depends on `pcre-ocaml`.


[mparser.mli]: https://bitbucket.org/mrm/mparser/src/1e78457d140f187a4119500fb7ad1c04b762fc44/src/mParser.mli?at=default
[charStream.mli]: https://bitbucket.org/mrm/mparser/src/1e78457d140f187a4119500fb7ad1c04b762fc44/src/charStream.mli?at=default

## Example From The MLI


This one uses the highest-level constructs: `expression` and `Token`:

- it evaluates the expression with `( * )`, `(/)`, etc.
- an expression-term is parentheses or a number: `parens expr <|> decimal`
*)
let example_from_the_mli () =
  let open MParser in
  let open Tokens in
  let infix sym f assoc = Infix  (skip_symbol sym >> return f, assoc) in
  let prefix sym f      = Prefix (skip_symbol sym >> return f) in
  let negate x = -x in
  let operators = [
    [ prefix "-" negate ];
    [ infix "*" ( * ) Assoc_left;
      infix "/" ( / ) Assoc_left ];
    [ infix "+" ( + ) Assoc_left;
      infix "-" ( - ) Assoc_left ]
  ] in
  let rec term s = (parens expr <|> decimal) s
  and expr s = expression operators term s in
  let eval s =
    match parse_string expr s () with
    | Success x ->
      say "%s → %d" s x
    | Failed (msg, _) ->
      say "%s → ERROR:\n%s" s msg
  in
  say "```";
  eval "42 + (34 - 12)";
  eval "42 +";
  eval "42 + (17 * (-22 @ 34)";
  say "```";
  say ""

let () = if_arg "mli" example_from_the_mli
(*doc

The error messages computed by the library are pretty cool, here is
the output: *)
(*result mli *)
(*doc
## More Basic Examples

These are mostly adapted from the [FParsec tutorial] (for F#).

[FParsec tutorial]: http://www.quanttec.com/fparsec/tutorial.html
*)

let basic_tests () =
  let open MParser in
  say "Some basic tests:\n";
  let test m s fmt =
    match m s with
    | Success v -> say ("    %-35S → OK: " ^^ fmt) s v
    | Failed (msg, No_error) -> say "NO ERROR:\n%s" msg
    | Failed (_, Parse_error ((idx,l,c), _)) ->
      say "    %-35S → ERROR at (%d, %d)" s l c
  in
  let parse_float s =
    let the_parser = Tokens.float in
    parse_string the_parser s ()
  in
  say "- parse_float:\n\n```";
  test parse_float "3.14" "%f";
  test parse_float "   3.14" "%f";
  test parse_float "3.14  " "%f";
  say "```";
  let parse_float_strip s =
    let the_parser = spaces >> Tokens.float in
    parse_string the_parser s ()
  in
  say "\n- parse_float (strip):\n\n```";
  test parse_float_strip "3.14" "%f";
  test parse_float_strip "   3.14" "%f";
  test parse_float_strip "3.14  " "%f";
  say "```";
  let parse_float_in_square_brackets s =
    let the_parser = spaces >> Tokens.squares Tokens.float in
    parse_string the_parser s ()
  in
  say "\n- parse `[float]`:\n\n```";
  test parse_float_in_square_brackets "[3.14]" "%f";
  test parse_float_in_square_brackets "[   3.14]" "%f";
  test parse_float_in_square_brackets "  [   3.14]" "%f";
  test parse_float_in_square_brackets "[3.14 ] " "%f";
  test parse_float_in_square_brackets "[ ] " "%f";
  test parse_float_in_square_brackets "[3.14 42 " "%f";
  say "```";
  let parse_float_list s =
    let the_parser =
      (spaces >>
       (Tokens.squares (Tokens.semi_sep_end Tokens.float))
       >>= fun fl ->
       return (String.concat ~sep:", " (List.map fl (sprintf "%.3f"))))
    in
    parse_string the_parser s ()
  in
  say "\n- parse `float list`:\n\n```";
  test parse_float_list "[3.14;4.2]" "%s";
  test parse_float_list "   [ 3.14;  4.2; 10  ] " "%s";
  test parse_float_list "   [ 3.14;\n 4.2;\n A  ] " "%s";
  test parse_float_list "   [ 3.14;  4.2; 10;  ] " "%s";
  say "```";
  let parse_float_or_string_list s =
    let the_parser =
      let float_or_string =
        (Tokens.float >>= fun f -> return (`float f))
        <|>
        (Tokens.string_literal >>= fun f -> return (`string f))
      in
      spaces >> Tokens.squares (Tokens.semi_sep_end float_or_string)
      >>= fun fl ->
       return (String.concat ~sep:", " (List.map fl (function
           | `float f -> sprintf "%.3f" f
           | `string s -> sprintf "%S" s)))
    in
    parse_string the_parser s ()
  in
  say "\n- parse `(float|string) list`:\n\n```";
  test parse_float_or_string_list "[3.14;4.2]" "%s";
  test parse_float_or_string_list "   [ 3.14;  \"4.2\"; 10  ] " "%s";
  test parse_float_or_string_list "   [ 3.14;\n 4.2;\n \"A  ] " "%s";
  test parse_float_or_string_list "   [ 3.14;  4.2; 10;  ] " "%s";
  say "```";
  let parse_float_list_or_string_list s =
    let the_float_list_parser =
      spaces >> (Tokens.squares (Tokens.semi_sep_end Tokens.float))
      >>= fun fl ->
      return (`float_list fl)
    in
    let the_string_list_parser =
      spaces >> (Tokens.squares (Tokens.semi_sep_end Tokens.string_literal))
      >>= fun fl ->
      return (`string_list fl)
    in
    let the_parser =
      (attempt the_float_list_parser) <|> the_string_list_parser
      >>= begin function
      | `float_list fl  -> return (String.concat ~sep:", " (List.map fl (sprintf "%.3f")))
      | `string_list sl -> return (String.concat ~sep:", " (List.map sl (sprintf "%S")))
      end
    in
    parse_string the_parser s ()
  in
  say "\n- parse `(float list|string list)`:\n\n```";
  test parse_float_list_or_string_list "[3.14;4.2]" "%s";
  test parse_float_list_or_string_list "   [ 3.14;  \"4.2\"; 10  ] " "%s";
  test parse_float_list_or_string_list "   [ 3.14;\n 4.2;\n \"A  ] " "%s";
  test parse_float_list_or_string_list "   [ \"3.14\"; \"AA\"; \"BB\"  ] " "%s";
  say "```";
  say ""

let () = if_arg "basic_tests" basic_tests
(*result basic_tests *)
