
#use "topfind";;
#thread;;
#camlp4o;;
#require "sequme";;
#require "sexplib.syntax";;

module Flow = struct
  include Sequme_flow
  include Sequme_flow_list
end
open Core.Std
open Flow

(*

  Some requirements:
  - handle meta-data (already typed?) and files
  - provide streams 'or' memory blobs
  - handle files on different machines

*)

type volume = {
  vid: int;
  location: string;
}
with sexp
  
type content =
| Int of int
| Record of (string * content) list
| Blob of [ `there of string | `volume of volume ]
with sexp
    
type item = {
  id: int;
  created: Time.t;
  last_modified: Time.t;
  content: content;
} 
with sexp

  
