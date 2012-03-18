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
