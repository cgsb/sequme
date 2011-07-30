#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "biocaml"; "sequme"]
--
open Batteries_uni;; open Printf;; open Biocaml;; open Sequme

(* Abbreviations:
   slid - a samples SL ID as defined by HudsonAlpha 
*)

let root = List.reduce Filename.concat ["/data"; "users"; "aa144"; "th17"]
let table = "fpkms" (* name of main table of FPKM values *)

let run cmd =
  (* printf "%s\n%!" cmd; *)
  Sys.command cmd |> ignore

let sqlite_sep db_file cmd =
  sprintf "sqlite3 -separator $'\t' %s \"%s\"" db_file cmd |> run
      
let sqlite db_file cmd =
  sprintf "sqlite3 %s \"%s\"" db_file cmd |> run

let expr_file slid = List.reduce Filename.concat
  [root; "cufflinks"; slid; "cufflinks_out"; slid ^ "_genes.expr"]

(** Create new table named [slid] with data for sample [slid]. *)
let add_table db_file slid =
  ["gene_id"; "bundle_id"; "chr"; "left"; "right"; "fpkm"; "fpkm_conf_lo"; "fpkm_conf_hi"; "status"]
  |> List.map (flip (^) " text")
  |> String.concat ", "
  |> sprintf "CREATE TABLE %s (%s)" slid
  |> sqlite db_file;
  sqlite_sep db_file (sprintf ".import %s %s" (expr_file slid) slid);
  sqlite db_file (sprintf "delete from %s where gene_id = \'gene_id\'" slid)

let drop_table db_file slid =
  sqlite db_file (sprintf "DROP TABLE %s" slid)


(** Create initial FPKMs table using data for sample [slid]. *)
let singleton db_file slid =
  add_table db_file slid
  ;
  let base_cols = ["gene_id"; "chr"; "left"; "right"] in
  let cols = base_cols@[slid] in
  sqlite db_file (
    sprintf "CREATE TABLE %s (%s, UNIQUE (%s))"
      table
      (String.concat ", " (List.map (flip (^) " text") cols))
      (String.concat "," base_cols)
  );
  let into = String.concat "," cols in
  let select = String.concat "," (base_cols@["fpkm"]) in
  sqlite db_file (
    sprintf "INSERT INTO %s (%s) SELECT %s FROM %s" table into select slid
  );
  drop_table db_file slid


(** Insert data for sample [slid] to FPKMs table. *)
let insert db_file slid =
  sqlite db_file (
    sprintf "ALTER TABLE %s ADD COLUMN %s text" table slid
  );
  let inp = expr_file slid |> open_in in
  let _,_,get,e = Table.of_input ~itags:"table,header,separator=\\t" inp in
  let update r =
    let fpkm = get r "FPKM" in
    let gene_id = get r "gene_id" in
    let chr = get r "chr" in
    let left = get r "left" in
    let right = get r "right" in
    sqlite db_file (
      sprintf "UPDATE %s SET %s='%s' WHERE gene_id='%s' AND chr='%s' AND left='%s' AND right='%s'"
        table slid fpkm gene_id chr left right
    )
  in
  Enum.iter update e;
  close_in inp

;;
type command = Init | Add

let genome = Sys.argv.(1) in

let db_file = List.reduce Filename.concat
  [
    root;
    "fpkms";
    match genome with
      | "mm9" -> "mm9.db"
      | "hg19" -> "hg19.db"
      | _ -> failwith (sprintf "unknown genome %s" genome)
  ]
in

let slids =
  let ans = ref [] in
  for i = 2 to Array.length Sys.argv - 1 do
    ans := Sys.argv.(i)::!ans
  done;
  List.rev !ans
in

let command =
  if Sys.file_exists db_file then Add else Init
in

let hd,tail = List.hd slids, List.tl slids in

(
  match command with
    | Init -> singleton db_file hd
    | Add -> insert db_file hd
);
List.iter (insert db_file) tail
