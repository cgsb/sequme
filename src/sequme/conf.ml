open Batteries_uni
module StringMap = Map.StringMap

exception Invalid of string

type t = string StringMap.t

let read sequme_root =
  Filename.concat (Filename.concat sequme_root "etc") "sequme.conf"
  |> File.lines_of
  |> Enum.map (flip String.split "=" |- Pair.map (String.strip |- Util.unquote))
  |> Enum.fold (flip (uncurry StringMap.add)) StringMap.empty
