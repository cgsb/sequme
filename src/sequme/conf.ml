open Batteries_uni
module StringMap = Map.StringMap

exception Invalid of string

type t = string StringMap.t

let read sequme_root =
  Filename.concat (Filename.concat sequme_root "etc") "sequme.conf"
  |> File.lines_of
  |> Enum.map (flip String.split "=" |- (fun (x,y) -> String.strip x, Util.unquote (String.strip y)))
  |> Enum.fold (flip (uncurry StringMap.add)) StringMap.empty
