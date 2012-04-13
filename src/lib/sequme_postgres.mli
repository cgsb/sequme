(** PostgreSQL support. The abstract syntax tree defined here meets
    only our specific needs. It is not a comprehensive representation
    of the PostgreSQL language, and the API allows creation of invalid
    SQL statements. *)

val exists_db : string -> bool
  (** [exists_db db] returns true if database [db] exists. Return
      value depends on other PostgreSQL environment variables such as
      PGHOST, PGUSER, etc.

      @raise Failure if unable to determine answer. This is quite
      possible as the implementation is highly fragile. Do not use
      this in production code.
  *)

(** Values *)
module Val : sig

  type t =
      | Int of int
      | Now

end

(** Column types *)
module Column : sig

  type base_type = 
      | SmallInt (** signed 2-byte integer *)
      | Integer (** signed 4-byte integer *)
      | BigInt (** signed 8-byte integer *)
      | Float4 (** single precision 4-byte float, aka real *)
      | Float8 (** double precision 8-byte float *)
      | Numeric of int * int
      | Char of int (** fixed-length string of length n *)
      | VarChar of int (** variable length string up to n bits *)
      | Text (** variable length string with no maximum length *)
      | Serial (** auto-incrementing 4-byte integer *)
      | BigSerial (** auto-incrementing 8-byte integer *)
      | Boolean
      | Bytea (** byte array *)
      | Money
      | Bit of int (** fixed-length bit string of length n *)
      | VarBit of int (** variable length bit string up to n bits *)
      | Date (** year, month, day *)
      | Time (** time of day *)
      | TimeStamp (** date and time *)
      | Interval (** time span *)
      | Cidr (** IPv4 or IPv6 network address *)
      | Inet (** IPv4 or IPv6 host address *)
      | MacAddr
      | Box
      | Circle
      | Line
      | LSeg
      | Path
      | Point
      | Polygon
      | TSQuery
      | TSVector
      | TxID_Snapshot
      | Uuid
      | Xml


  type typ =
      | Scalar of base_type
      | Array of base_type

  (** Column constraints. Not called "constraint" because that is an
      OCaml keyword. *)
  type modifier =
      | NotNull
      | Unique
      | PrimaryKey
      | Default of Val.t
      | WithTimeZone
      | References of string * string (** foreign key reference to (table,column) *)

  (** A column declaration consists of a column name, column type, and
      a list of constraints. *)
  type decl = string * typ * modifier list

  val smallint : typ
  val integer : typ
  val bigint : typ
  val float4 : typ
  val real : typ
  val float8 : typ
  val double_precision : typ
  val numeric : int -> int -> typ
  val char : int -> typ
  val varchar : int -> typ
  val text : typ
  val serial : typ
  val bigserial : typ
  val boolean : typ
  val bytea : typ
  val money : typ
  val bit : int -> typ
  val varbit : int -> typ
  val date : typ
  val time : typ
  val timestamp : typ
  val interval : typ
  val cidr : typ
  val inet : typ
  val macaddr : typ
  val box : typ
  val cirlce : typ
  val line : typ
  val point : typ
  val lseg : typ
  val path : typ
  val point : typ
  val polygon : typ
  val tsquery : typ
  val tsvector : typ
  val txid_snapshot : typ
  val uuid : typ
  val xml : typ

end

(** Tables *)
module Table : sig

  (** A table declaration. *)
  type decl = private {
    name : string; (** name of the table *)
    temporary : bool; (** true if the table is temporary *)
    columns : Column.decl list; (** columns of the table *)
  }

  (** [make name columns] returns a table type. Default value for
      [temporary] is false.

      @raise Failure if given parameters do not represent a valid
      table. (WARNING: currently no checks are being done.)
  *)
  val make : name:string -> ?temporary:bool -> columns:(Column.decl list) -> unit -> decl

  (** Given a table declaration, return the PostgreSQL statement to
      create it. *)
  val create_table_stmt : decl -> string

end
