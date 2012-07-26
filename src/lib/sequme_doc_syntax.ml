open Core.Std

module Xml_tree = struct
  include Xmlm
  let in_tree i = 
    let el tag childs = `E (tag, childs)  in
    let data d = `D d in
    input_doc_tree ~el ~data i
end

type inline =
| Bold of inline list
| Italic of inline list
| Code of inline list 
| Link of [`url of string] list * inline list
| Text of string

type structural =
| Section of [`one|`two|`three|`four] * string option * inline list
| Code_bloc of [`language of string] list * inline list
| Paragraph
| Line_break
| Inline of inline list
| Numbered_list of [`item of structural list] list
| Unnumbered_list of [`item of structural list] list
    
type document = structural list
    
open Result

type parsing_error = [
| `unknown_xml_tag of string
| `xml_syntax_error of Xml_tree.pos * Xml_tree.error
| `unexpected_text of string
]
exception Parsing_error of parsing_error

let parse ?(pedantic=true) file_content =
  begin try
          let content = "<content>" ^ file_content ^ "</content>" in
          return Xml_tree.(in_tree (make_input (`String (0, content))))
    with
    | Xml_tree.Error (p, e) -> fail (`xml_syntax_error (p, e))
  end
  >>= fun xml ->
  let continue l f = List.map l f |! List.concat in
  let find_attr name attrs =
    List.find_map attrs (fun ((_, attr), value) ->
      if name = attr then Some value else None) in
  let rec go_through_inline = function
    | `E (((_, "b"), _), inside) -> [Bold (continue inside go_through_inline)]
    | `E (((_, "i"), _), inside) -> [Italic (continue inside go_through_inline)]
    | `E (((_, "t"), _), inside) -> [Code (continue inside go_through_inline)]
    | `E (((_, "link"), attr), inside) ->
      let options = List.filter_opt [
        Option.map (find_attr "url" attr) (fun s -> `url s) ] in
      [Link (options, continue inside go_through_inline)]
    | `E (((_, unknown), _), tl) ->
      if pedantic then
        raise (Parsing_error (`unknown_xml_tag unknown))
      else
        continue tl go_through_inline 
    | `D s -> [Text s]
  in
  let rec go_through_list = function
    | `E (((_,"item"), attrs), inside) -> [`item (continue inside go_through)]
    | `E (((_, unknown), _), tl) ->
      if pedantic then raise (Parsing_error (`unknown_xml_tag unknown))
      else continue tl go_through_list 
    | `D s ->
      if pedantic then raise (Parsing_error (`unexpected_text s)) else []
  and go_through = function
    | `E (((_,"content"), _), inside) -> continue inside go_through
    | `E (((_, h_something), attrs), inside)
        when h_something = "h1" || h_something = "h2"
            || h_something = "h3" || h_something = "h4" ->
      let id = find_attr "id" attrs in
      let level =
        match String.(sub h_something 1 (length h_something - 1)) with
        | "1" -> `one | "2" -> `two | "3" -> `three | "4" -> `four
        | s -> raise (Parsing_error (`unknown_xml_tag h_something)) in
      [Section (level, id, continue inside go_through_inline)]
    | `E (((_,"p"), attrs), inside) -> [Paragraph]
    | `E (((_,"br"), attrs), inside) -> [Line_break]
    | `E (((_,"list"), attrs), inside) ->
      [Unnumbered_list (continue inside go_through_list)]
    | `E (((_,"enum"), attrs), inside) ->
      [Numbered_list (continue inside go_through_list)]
    | `E (((_,"code"), attrs), inside) ->
      let options = List.filter_opt [
        Option.map (find_attr "language" attrs) (fun s -> `language s) ] in
      [Code_bloc (options, continue inside go_through_inline)]
    | `E (t, tl) as e -> [Inline (go_through_inline e)]
    | `D s -> [Inline [Text s]]
  in
  try
    return (go_through (snd xml) : document)
  with
    Parsing_error e -> fail e


let to_html ?(map_section_levels=ident) (v: document) =
  let buf = Buffer.create 42 in
  let str s = Buffer.add_string buf s in
  let chr c = Buffer.add_char buf c in
  let strf fmt = ksprintf (Buffer.add_string buf) fmt in 
  let rec inline = function
    | Bold inside -> 
      str "<b>"; List.iter inside inline; str "</b>"
    | Italic inside ->
      str "<i>"; List.iter inside inline; str "</i>"
    | Code  inside ->
      str "<code>"; List.iter inside inline; str "</code>"
    | Link (options, inside) ->
      strf "<a %s>"
        (List.map options (function `url url -> sprintf "href=%S" url)
         |! String.concat ~sep:" ");
      List.iter inside inline;
      str "</a>"
    | Text s ->
      String.iter s (function
      | '<' -> str "&lt;"
      | '>' -> str "&gt;"
      | '&' -> str "&amp;"
      | c -> chr c)
  in
  let rec structural = function
    | Section (level, idopt, inside) ->
      let level_int =
        (match level with `one -> 1 | `two -> 2 | `three -> 3 | `four -> 4)
        |! map_section_levels
      in
      strf "</p><h%d%s>" level_int
        (Option.value_map idopt ~default:"" ~f:(sprintf " id=%S"));
      List.iter inside inline;
      strf "</h%d>\n<p>" level_int
    | Code_bloc (options, inside) ->
      str "</p>\n<pre>";
      List.iter inside inline;
      str "</pre><p>"
    | Paragraph -> str "</p><p>"
    | Line_break -> str "<br/>"
    | Inline i ->  List.iter i inline
    | Numbered_list items ->
      str "</p><ol>\n";
      List.iter items (function
      | `item ls -> str "<li><p>"; List.iter ls structural; str "</p></li>\n");
      str "</ol>\n<p>";
    | Unnumbered_list items ->
      str "</p><ul>\n";
      List.iter items (function
      | `item ls -> str "<li><p>"; List.iter ls structural; str "</p></li>\n");
      str "</ul>\n<p>";
  in
  str "<p>";
  List.iter v structural;
  str "</p>";
  Buffer.contents buf
