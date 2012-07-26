
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

type parsing_error = [
| `unknown_xml_tag of string
| `xml_syntax_error of Xmlm.pos * Xmlm.error
| `unexpected_text of string
]
val parse: ?pedantic:bool -> string -> (document, parsing_error) Core.Std.Result.t

val to_html: ?map_section_levels:(int -> int) -> document -> string
