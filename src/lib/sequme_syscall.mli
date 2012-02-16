(** System calls. Support for making system calls.

    WARNING: It is unknown whether this module works on Windows.
*)

val syscall : ?path:string -> string -> (string * string * Unix.process_status)
  (** [syscall cmd] executes the system command [cmd], returning the
      contents of stdout, stderr, and the process status. The command
      is executed in an environment with only the following
      environment variables:

      - path - Default is the value of PATH in the environment from
               which [syscall] is called.

  *)
