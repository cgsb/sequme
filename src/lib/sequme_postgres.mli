(** PostgreSQL support. The abstract syntax tree defined here meets
    only our specific needs. It is not a comprehensive representation
    of the PostgreSQL language, and the API makes only minimal effort
    to enforce SQL's type system.
*)

val exists_db : string -> bool
  (** [exists_db db] returns true if database [db] exists. Return
      value depends on other PostgreSQL environment variables such as
      PGHOST, PGUSER, etc.

      @raise Failure if unable to determine answer. This is quite
      possible as the implementation is highly fragile. Do not use
      this in production code.
  *)

(** [exec dbh query] will execute [query] against the database handle
    [dbh]. *)
val exec : 'a PGOCaml.t -> string -> PGOCaml.row list

(** Values *)
module Val : sig

  (** OCaml representation of a SQL value depends on the column
      modifiers used. An ['a rep] embodies the different representations
      needed. *)
  type 'a rep =
      | Scalar of 'a
      | ScalarOpt of 'a option
      | Array of 'a array
      | ArrayOpt of 'a array option

  type bytea = string
      (** Byte arrays in OCaml are normal strings. *)

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

  (** Human readable string representation of given value. No
      guarantee this can be parsed back into a {!t}. *)
  val to_string : t -> string

end

(** Column types *)
module Column : sig

  type typ = 
      | SmallInt (** signed 2-byte integer *)
      | Integer (** signed 4-byte integer *)
      | BigInt (** signed 8-byte integer *)
      | Float4 (** single precision 4-byte float, aka real *)
      | Float8 (** double precision 8-byte float *)
      | Numeric of int * int (** exact decimal with given precision (total count of significant digits ) and scale (count of decimal digits in the fractional part) *)
      | Char of int (** fixed-length string of length n *)
      | VarChar of int (** variable length string up to length n *)
      | Text (** variable length string with no maximum length *)
      | Boolean
      | Bytea (** byte array *)
      | Money
      | Bit of int (** fixed-length bit string of length n *)
      | VarBit of int (** variable length bit string up to n bits *)
      | Date (** year, month, day *)
      | Time (** time *)
      | Timestamp (** date and time *)
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

  (** Column modifiers (also called constraints). *)
  type modifier =
      | Array
      | NotNull
      | Unique
      | PrimaryKey
      | Default of Val.t
      | DefaultNow (** only valid with date and time types *)
      | WithTimeZone (** only valid with time types *)
      | Serial (** only valid with {!Integer} or {!BigInt} *)
      | References of string * string (** foreign key reference to (table,column) *)

  (** A column declaration. *)
  type decl = private {
    name : string; (** column name *)
    typ : typ; (** column type *)
    modifiers : modifier list; (** column modifiers *)
  }

  (** Make a [decl]. Duplicate modifiers are simply discarded.
      
      @raise Failure if given parameters do not correspond to a valid
      declaration.  *)
  val make_decl : name:string -> typ:typ -> modifiers:(modifier list) -> decl

  (** SQL string for given [typ]. *)
  val typ_to_string : typ -> string

  (** SQL string for given [modifier]. 

      @raise Failure if given {!Array} or {!Serial}, neither of which
      are printed within modifier section of SQL syntax. *)
  val modifier_to_string : modifier -> string

  (** SQL string representation for a column declaration, especially
      as used within CREATE TABLE statements. *)
  val decl_to_string : decl -> string

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
      table.
  *)
  val make : name:string -> ?temporary:bool -> columns:(Column.decl list) -> unit -> decl

  (** Given a table declaration, return the PostgreSQL statement to
      create it. *)
  val create_table_stmt : decl -> string

end

(** Escape any string to the byte-array PostgreSQL type. *)
module Bytea: sig

  (** Encode any string with Postgresql 8.2 'Escape' format. *)
  val to_db_input: string -> string

  (** Decode the output of a query (from either Postgresal 8.2 'Escape'
      format or from Postgresql 9.0 'Hex' format). *)
  val of_db_output: string -> string

end
