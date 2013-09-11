{
open Printf
open Purel_parser
}

let eol = '\n' | ('\r' '\n')
let var = ['a'-'z'] ['a'-'z' 'A' - 'Z' '0' - '9']*
let int = ['0'-'9']+
let string = '\"' (_*) '\"'

rule token = parse
| eol           { token lexbuf }
| [' ' '\t']    { token lexbuf }
| int as i      { INT (int_of_string i) }
| string as x   { STRING x }
| "true"        { TRUE }
| "false"       { FALSE }
| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { TIMES }
| "not"         { NOT }
| "and"         { AND }
| "or"          { OR }
| ','           { COMMA }
| ';'           { SEMICOLON }
| '='           { EQUAL }
| '('           { LPAREN }
| ')'           { RPAREN }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| '{'           { LBRACE }
| '}'           { RBRACE }
| var as x      { VAR x }
| eof           { EOF }
| _ as x
    { raise (Failure (
      sprintf "At offset %d: unexpected character %c"
        (Lexing.lexeme_start lexbuf)
        x
    ))
    }
