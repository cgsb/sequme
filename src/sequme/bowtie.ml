open Batteries_uni;; open Printf

let path_of_index conf i =
  let sequme_root = Map.StringMap.find "sequme_root" conf in
  let path = List.fold_left Filename.concat "" [sequme_root; "db"; "bowtie"; "indexes"; i] in
  if Sys.file_exists path then path
  else raise Not_found
