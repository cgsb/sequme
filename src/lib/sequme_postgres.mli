(** PostgreSQL support. *)

val exists_db : string -> bool
  (** [exists_db db] returns true if database [db] exists. Return
      value depends on other PostgreSQL environment variables such as
      PGHOST, PGUSER, etc.

      @raise Failure if unable to determine answer. This is quite
      possible as the implementation is highly fragile. Do not use
      this in production code.
  *)
