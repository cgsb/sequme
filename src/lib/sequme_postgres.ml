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
  type t = Int of int | Now

  let to_string = function
    | Int x -> string_of_int x
    | Now -> "now()"

end

module Column = struct

  type base_type = 
      | SmallInt | Integer | BigInt
      | Float4 | Float8 | Numeric of int * int
      | Char of int | VarChar of int | Text
      | Serial | BigSerial
      | Boolean | Bytea | Money
      | Bit of int | VarBit of int
      | Date | Time | TimeStamp | Interval
      | Cidr | Inet | MacAddr
      | Box | Circle | Line | LSeg | Path | Point | Polygon
      | TSQuery | TSVector | TxID_Snapshot
      | Uuid | Xml

  type typ =
      | Scalar of base_type
      | Array of base_type

  type modifier =
      | NotNull
      | Unique
      | PrimaryKey
      | Default of Val.t
      | WithTimeZone
      | References of string * string (** foreign key reference to (table,column) *)

  type decl = string * typ * modifier list

  let smallint = Scalar SmallInt
  let integer = Scalar Integer
  let bigint = Scalar BigInt
  let float4 = Scalar Float4
  let real = float4
  let float8 = Scalar Float8
  let double_precision = Scalar Float8
  let numeric m n = Scalar (Numeric (m,n))
  let char n = Scalar (Char n)
  let varchar n = Scalar (VarChar n)
  let text = Scalar Text
  let serial = Scalar Serial
  let bigserial = Scalar BigSerial
  let boolean = Scalar Boolean
  let bytea = Scalar Bytea
  let money = Scalar Money
  let bit n = Scalar (Bit n)
  let varbit n = Scalar (VarBit n)
  let date = Scalar Date
  let time = Scalar Time
  let timestamp = Scalar TimeStamp
  let interval = Scalar Interval
  let cidr = Scalar Cidr
  let inet = Scalar Inet
  let macaddr = Scalar MacAddr
  let box = Scalar Box
  let cirlce = Scalar Circle
  let line = Scalar Line
  let point = Scalar Point
  let lseg = Scalar LSeg
  let path = Scalar Path
  let point = Scalar Point
  let polygon = Scalar Polygon
  let tsquery = Scalar TSQuery
  let tsvector = Scalar TSVector
  let txid_snapshot = Scalar TxID_Snapshot
  let uuid = Scalar Uuid
  let xml = Scalar Xml

  let base_type_to_string = function
    | SmallInt -> "smallint"
    | Integer -> "integer"
    | BigInt -> "bigint"
    | Float4 -> "real"
    | Float8 -> "double precision"
    | Numeric (m,n) -> sprintf "numeric (%d,%d)" m n
    | Char n -> sprintf "character (%d)" n
    | VarChar n -> sprintf "character varying (%d)" n
    | Text -> "text"
    | Serial -> "serial"
    | BigSerial -> "bigserial"
    | Boolean -> "boolean"
    | Bytea -> "bytea"
    | Money -> "money"
    | Bit n -> sprintf "bit (%d)" n
    | VarBit n -> sprintf "bit varying (%d)" n
    | Date -> "date"
    | Time -> "time"
    | TimeStamp -> "timestamp"
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

  let typ_to_string = function
    | Scalar x -> base_type_to_string x
    | Array x -> sprintf "%s[]" (base_type_to_string x)

  let modifier_to_string = function
    | NotNull -> "not null"
    | Unique -> "unique"
    | PrimaryKey -> "primary key"
    | Default x -> sprintf "default %s" (Val.to_string x) 
    | WithTimeZone -> "with time zone"
    | References (table,column) -> sprintf "references %s(%s)" table column

  let decl_to_string (name,typ,modifiers) =
    sprintf "\"%s\" %s%s%s"
      name
      (typ_to_string typ)
      (if List.length modifiers > 0 then " " else "")
      (modifiers |> List.map ~f:modifier_to_string |> String.concat ~sep:" ")

end

module Table = struct

  type decl = {
    name : string;
    temporary : bool;
    columns : Column.decl list;
  }

  let make ~name ?(temporary=false) ~columns () =
    {name; temporary; columns}

  let create_table_stmt x =
    sprintf "CREATE %sTABLE %s (%s)"
      (if x.temporary then "TEMP " else "")
      x.name
      (x.columns |> List.map ~f:Column.decl_to_string |> String.concat ~sep:", ")

end
