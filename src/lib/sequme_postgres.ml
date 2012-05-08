open Sequme_std
module Syscall = Sequme_syscall

let exists_db db =
  let err_str = sprintf "psql: FATAL:  database \"%s\" does not exist" db in
  let cmd = sprintf "psql -t -c \"SELECT COUNT(1) FROM pg_catalog.pg_database WHERE datname = '%s'\"" db in
  let stdout,stderr,exit_status = Syscall.syscall cmd in
  Syscall.check_exit_status ~process:"psql" exit_status;
  let stdout = String.strip stdout in
  if stdout = "1" then true
  else if stdout = "0" then false
  else if stderr = err_str then false
  else failwith (sprintf "Sequme_postgres.exists_db returned\n  stdout: %s\n  stderr: %s\n" stdout stderr)

module Val = struct

  type 'a rep =
      | Scalar of 'a
      | ScalarOpt of 'a option
      | Array of 'a array
      | ArrayOpt of 'a array option

  type bytea = string

  type t =
      | SmallInt of int rep
      | Integer of int32 rep
      | BigInt of int64 rep
      | Float4 of float rep
      | Float8 of float rep
      | Char of string rep * int (** string value, plus its fixed length *)
      | VarChar of string rep * int (** string value, plus its max length *)
      | Text of string rep
      | Boolean of bool rep
      | Bytea of bytea rep
      | Date of Core.Std.Date.t rep
      | Timestamptz of Core.Std.Time.t rep (** time with time zone *)
      | Interval of Core.Span.t rep
      (* Values of other types not supported because unclear what
         OCaml type should represent them. Note, PG'OCaml uses
         float for Numeric but that does not obey the required SQL
         semantics.*)

  let rep_to_string a_to_string x =
    let array =
      Array.to_list
    |- List.map ~f:a_to_string
    |- String.concat ~sep:", "
    |- sprintf "{%s}" 
    in
    match x with
      | Scalar x -> a_to_string x
      | ScalarOpt x -> (match x with
          | Some x -> sprintf "SOME %s" (a_to_string x)
          | None -> "NONE"
        )
      | Array x -> array x
      | ArrayOpt x -> (match x with
          | Some x -> sprintf "SOME %s" (array x)
          | None -> "NONE"
        )

  let to_string = function
    | SmallInt x -> rep_to_string string_of_int x
    | Integer x -> rep_to_string Int32.to_string x
    | BigInt x -> rep_to_string Int64.to_string x
    | Float4 x -> rep_to_string string_of_float x
    | Float8 x -> rep_to_string string_of_float x
    | Char (x,_) -> rep_to_string (fun x -> x) x
    | VarChar (x,_) -> rep_to_string (fun x -> x) x
    | Text x -> rep_to_string (fun x -> x) x
    | Boolean x -> rep_to_string string_of_bool x
    | Bytea x -> rep_to_string (fun x -> x) x
    | Date x -> rep_to_string (Core.Std.Date.sexp_of_t |- Sexp.to_string_mach) x
    | Timestamptz x -> rep_to_string Core.Std.Time.to_string_abs x
    | Interval x -> rep_to_string Core.Span.to_string x

end

module Column = struct

  type typ = 
      | SmallInt | Integer | BigInt
      | Float4 | Float8 | Numeric of int * int
      | Char of int | VarChar of int | Text
      | Boolean | Bytea | Money
      | Bit of int | VarBit of int
      | Date | Time | Timestamp | Interval
      | Cidr | Inet | MacAddr
      | Box | Circle | Line | LSeg | Path | Point | Polygon
      | TSQuery | TSVector | TxID_Snapshot
      | Uuid | Xml

  type modifier =
      | Array
      | NotNull
      | Unique
      | PrimaryKey
      | Default of Val.t
      | DefaultNow
      | WithTimeZone
      | Serial
      | References of string * string

  type decl = {name : string; typ : typ; modifiers : modifier list;}

  let typ_to_string = function
    | SmallInt -> "smallint"
    | Integer -> "integer"
    | BigInt -> "bigint"
    | Float4 -> "real"
    | Float8 -> "double precision"
    | Numeric (m,n) -> sprintf "numeric (%d,%d)" m n
    | Char n -> sprintf "character (%d)" n
    | VarChar n -> sprintf "character varying (%d)" n
    | Text -> "text"
    | Boolean -> "boolean"
    | Bytea -> "bytea"
    | Money -> "money"
    | Bit n -> sprintf "bit (%d)" n
    | VarBit n -> sprintf "bit varying (%d)" n
    | Date -> "date"
    | Time -> "time"
    | Timestamp -> "timestamp"
    | Interval -> "interval"
    | Cidr -> "cidr"
    | Inet -> "inet"
    | MacAddr -> "macaddr"
    | Box -> "box"
    | Circle -> "circle"
    | Line -> "line"
    | LSeg -> "lseg"
    | Path -> "path"
    | Point -> "point"
    | Polygon -> "polygon"
    | TSQuery -> "tsquery"
    | TSVector -> "tsvector"
    | TxID_Snapshot -> "txid_snapshot"
    | Uuid -> "uuid"
    | Xml -> "xml"

  let modifier_to_string = function
    | Array -> failwith "modifier_to_string not defined for Array"
    | NotNull -> "NOT NULL"
    | Unique -> "UNIQUE"
    | PrimaryKey -> "PRIMARY KEY"
    | Default x -> sprintf "DEFAULT %s" (Val.to_string x) 
    | DefaultNow -> "DEFAULT now()"
    | WithTimeZone -> "WITH TIME ZONE"
    | Serial -> failwith "modifier_to_string not defined for Serial"
    | References (table,column) -> sprintf "REFERENCES %s(%s)" table column

  let decl_to_string {name;typ;modifiers} =
    let typ,modifiers =
      if List.mem Serial modifiers then
        let typ = (match typ with
          | Integer -> "serial"
          | BigInt -> "bigserial"
          | _ -> assert false
        )
        in
        typ, List.remove modifiers Serial
      else
        typ_to_string typ, modifiers
    in
    let typ,modifiers =
      if List.mem Array modifiers
      then typ ^ "[]", List.remove modifiers Array
      else typ, modifiers
    in
    let padding = if List.length modifiers > 0 then " " else "" in
    let modifiers =
      modifiers
    |> List.map ~f:modifier_to_string
    |> String.concat ~sep:" "
    in
    sprintf "\"%s\" %s%s%s" name typ padding modifiers

  let make_decl ~name ~typ ~modifiers =
    let modifiers = List.unique modifiers in
    let decl = {name; typ; modifiers} in (* define here to allow use in error messages *)
    let decl_str = decl_to_string decl in
    let typ_str = typ_to_string typ in
    List.iter modifiers ~f:(fun modifier ->
      let mod_str = modifier_to_string modifier in
      match modifier with
        | Array | NotNull
        | Unique | PrimaryKey
        | References _ -> ()
        | Default _ -> () (* TODO: check that default value is of correct type *)
        | DefaultNow -> (match typ with
            | Date | Time | Timestamp -> ()
            | _ -> failwith (sprintf "%s invalid for non-time column type %s" mod_str typ_str)
          )
        | WithTimeZone -> (match typ with
            | Time | Timestamp -> ()
            | _ -> failwith (sprintf "%s invalid for non-time column type %s" mod_str typ_str)
          )
        | Serial ->
            (match typ with
              | Integer
              | BigInt -> ()
              | _ -> failwith (sprintf "serial not valid with column type %s" typ_str)
            );
            if List.mem Array modifiers then
              failwith (sprintf "serial not valid with array in column declaration %s" decl_str)
    );
    decl

end

module Table = struct

  type decl = {
    name : string;
    temporary : bool;
    columns : Column.decl list;
  }

  let make ~name ?(temporary=false) ~columns () =
    let _ = List.fold_left columns ~init:[] ~f:(fun ans x ->
      if List.mem x.Column.name ans then
        failwith (sprintf "duplicate column name %s" x.Column.name)
      else
        x.Column.name::ans
    )
    in    
    {name; temporary; columns}

  let create_table_stmt x =
    sprintf "CREATE %sTABLE %s (%s)"
      (if x.temporary then "TEMP " else "")
      x.name
      (x.columns |> List.map ~f:Column.decl_to_string |> String.concat ~sep:", ")

end
