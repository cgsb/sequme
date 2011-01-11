open Batteries_uni;; open Printf

type status_html = string
type headers = string list
type data_rows = string list list
type table = headers * data_rows
type libid = string

module Nethtml = struct
  include Nethtml

  let get_Element = function
    | Data _ -> failwith "get_Element given Data node"
    | Element x -> x

  let fold_dfs (f : int -> document -> 'a -> 'a) (init : 'a) (doc : document) : 'a =
    let ans = ref init in
    let rec helper depth doc = match doc with
      | Element (_, _, child_docs) ->
          ans := f depth doc !ans;
          List.iter (helper (depth+1)) child_docs
      | Data _ ->
          ans := f depth doc !ans
    in
    helper 1 doc;
    !ans
      
  let iter_dfs (f : int -> document -> unit) (doc : document) : unit =
    fold_dfs (fun depth content () -> f depth content) () doc

  (** Returns [n]th element with given [name], where [doc] is
      traversed in depth-first order. *)
  let get_nth_element_with_name n name doc : (document, string) result =
    let f _ curr_node ((num_seen, prev_node) as accum) = match curr_node with
      | Data _ -> accum
      | Element (name', _, _) ->
          if name = name' then
            if num_seen = n - 1 then
              (num_seen+1, Some curr_node)
            else
              (num_seen+1, prev_node)
          else
            accum
    in
    let num_seen, node = fold_dfs f (0,None) doc in
    if num_seen < n then
      Bad (sprintf "requested %dth occurrence of element %s but found only %d" n name num_seen)
    else
      match node with
        | Some x -> Ok x
        | None -> assert false

  (** Return contents of [n]th Data node or Bad if fewer than [n] Data nodes found. *)
  let get_nth_data n doc : (string, string) result =
    let f _ curr_node ((num_seen, prev_node) as accum) = match curr_node with
      | Data _ ->
          if num_seen = n - 1 then
            (num_seen+1, Some curr_node)
          else
            (num_seen+1, prev_node)
      | Element _ -> accum
    in
    let num_seen, node = fold_dfs f (0,None) doc in
    if num_seen < n then
      Bad (sprintf "requested %dth Data node but found only %d" n num_seen)
    else
      match node with
        | Some (Data x) -> Ok x
        | Some _ | None -> assert false
         
  (** Returns contents of first Data node found within given
      [doc]. Returns empty string if none found. *)
  let get_data doc : string =
    match get_nth_data 1 doc with
      | Bad msg -> ""
      | Ok data -> data
          
  let get_table doc : table =
    match doc with
      | Data _ ->
          failwith "get_table_data applied to Data node"
            
      | Element ("table", _, header::rest) ->
          (* parse header row *)
          let is_th_elt = function Element ("th",_,_) -> true | _ -> false in
          let header =
            match header with
              | Element ("tr",_,child_docs) -> child_docs |> List.filter is_th_elt |> List.map get_data
              | _ -> failwith "invalid format for header row"
          in
          
          (* parse data rows *)
          let parse_single_data_row = function
            | Element ("tr",_,child_docs) -> child_docs |> List.map get_data
            | _ -> failwith "invalid format for data row"
          in
          let data = List.map parse_single_data_row rest in
          header, data

      | Element ("table", _, _) ->
          failwith "table node expected to contain a header row followed by zero or more data rows"

      | Element (name,_,_) ->
          failwith (sprintf "get_table_data given Element %s node" name)

end

(* for debugging *)
let print_short_info depth content =
  let padding = String.init (2 * (depth-1)) (fun _ -> ' ') in
  match content with
    | Nethtml.Element (name,_,_) ->
        printf "%s%s\n" padding name
    | Nethtml.Data x ->
        let x = String.left x (min 25 (String.length x)) |> String.filter ((!=) '\n') in
        printf "%sData: %s\n" padding x

let get_status_html passwd =
  let cookie_file = Filename.temp_file "hudsonalpha" "cookie.txt" in
  let status_file = Filename.temp_file "hudsonalpha" "status.html" in
  sprintf "curl -i --cookie-jar %s http://hts.hudsonalpha.org/status/ > /dev/null 2> /dev/null" cookie_file |> Sys.command |> ignore;
  sprintf "curl -i --cookie %s --cookie-jar %s --data \"username=LittmanLab&password=%s&this_is_the_login_form=1&post_data=\" http://hts.hudsonalpha.org/status/ > /dev/null 2> /dev/null" cookie_file cookie_file passwd |> Sys.command |> ignore;
  sprintf "curl --cookie %s --cookie-jar %s http://hts.hudsonalpha.org/status/ 2> /dev/null > %s" cookie_file cookie_file status_file |> Sys.command |> ignore;
  let cin = open_in status_file in
  let ans = IO.read_all cin in
  close_in cin;
  Unix.unlink cookie_file;
  Unix.unlink status_file;
  ans

let get_marias_table status_html =
  let doc = status_html |> IO.input_string |> Lexing.from_channel
    |> Nethtml.parse_document |> (flip List.nth 1)
  in
  let snd_table = match Nethtml.get_nth_element_with_name 2 "table" doc with
    | Ok x -> x
    | Bad msg -> failwith msg
  in
  Nethtml.get_table snd_table

let get_sequenced_libids (headers,data) =
  let libid_idx = 4 in
  let status_idx = 6 in
  assert (List.nth headers libid_idx = "Lib Id");
  assert (List.nth headers status_idx = "Status");
  let pred row = List.nth row status_idx = "Sequenced" in
  let sequenced_rows = List.filter pred data in
  List.map (flip List.nth libid_idx) sequenced_rows

let cmp_libid x y =
  let f = flip String.split "L" |- snd |- int_of_string in
  compare (f y) (f x)

let download_fastq dir id =
  let url = sprintf "http://mendel.hudsonalpha.org/qnU5XmtUBMtS/%s/%s.fastq.gz" id id in
  sprintf "mkdir -p %s; cd %s; wget %s" dir dir url |> Sys.command |> ignore

let fastq_path_of_libid conf libid =
  let sequme_root = Map.StringMap.find "sequme_root" conf in
  let path = List.fold_left (^) "" [sequme_root; "db"; "hudsonalpha"; libid; sprintf "%s.fastq" libid] in
  if Sys.file_exists path then path
  else raise Not_found

(* Create a PBS script to download a dataset. DEPRECATED. *)
let pbs_run id =
  let url = sprintf "http://mendel.hudsonalpha.org/qnU5XmtUBMtS/%s/%s.fastq.gz" id id in
  let dir = Filename.concat "/data/sequme/hudsonalpha" id in
  let pbs_script_name = sprintf "/data/sequme/log/20101214_%s_download.pbs" id in
  let script = String.concat "\n" [
    "#PBS -V";
    "#PBS -r n";
    "#PBS -l nodes=1:ppn=8";
    sprintf "#PBS -o /data/sequme/log/20101214_%s_download.pbs.log" id;
    "#PBS -j oe";
    sprintf "#PBS -N %s_download" id;
    "";
    sprintf "mkdir -p %s; cd %s; wget %s" dir dir url;
    ""]
  in
  let cout = open_out pbs_script_name in
  fprintf cout "%s" script;
  close_out cout;
  sprintf "qsub %s" pbs_script_name |> Sys.command |> ignore
