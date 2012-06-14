(** Certificate authority (for now, wrap [openssl]). *)
open Core.Std
  
type t


val create :
  ?openssl_command:string ->
  ?dn_country:string ->
  ?dn_province:string ->
  ?dn_city:string ->
  ?dn_org:string ->
  ?dn_orgunit:string ->
  ?dn_email:string ->
  ?rsa_key_size:int ->
  ?default_validity:int ->
  ?ca_filename_prefix:string ->
  ?ca_cn:string ->
  string -> t
(** Create a [t] associated with a directory path (should be absolute). *)

val establish :
  t ->
  (unit,
   [> `system_command_error of
       string *
         [> `exited of int
         | `exn of exn
         | `signaled of int
         | `stopped of int ]
   | `write_file_error of string * exn ])
    Sequme_flow.t
(** Install a certificate authority in its directory. *)

val load :
  string ->
  (t, [> `parse_config_error of exn
      | `read_file_error of string * exn ]) Sequme_flow.t
(** Load a certificate-authority from a given path. *)

    
val make_server_certificate : t -> name:string ->
  (unit,
   [> `system_command_error of
       string *
         [> `exited of int
         | `exn of exn
         | `signaled of int
         | `stopped of int ]
   | `write_file_error of string * exn ]) Sequme_flow.t
(** Create a certificate for a server of common name [name].
    If [name] is already used a new certificate is created for this
    “person”, if not a new entity is created.
*)

val server_crtkey_path : t -> name:string ->
  (string,
   [> `certificate_revoked of string * Time.t
   | `server_not_found of string ]) Result.t
(** Get the path to the 'crtkey' file of the server [name]. *)

val server_certificate_and_key_paths: t -> name:string ->
  (string * string,
   [> `certificate_revoked of string * Time.t
   | `server_not_found of string ]) Result.t
(** Get the paths to the 'crt' and 'key' files of the server [name]. *)

val server_history: t -> name: string ->
  (string *
     [ `created of Core.Std.Time.t
     | `revoked of Core.Std.Time.t ] list) list option
(** Get the certification history of a given server. *)
