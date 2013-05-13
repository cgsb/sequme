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
  (t,
   [> `parse_config_error of exn
   | `read_file_error of string * exn
   | `read_file_timeout of string * float ]) Sequme_flow.t
(** Load a certificate-authority from a given path. *)

val ca_certificate_path: t -> string
(** Get the path to the CA's certificate. *)

val make_certificate:
  ?verbose:bool -> t -> kind:[`server|`client] -> name:string ->
  (unit,
   [> `system_command_error of
       string *
         [> `exited of int
         | `exn of exn
         | `signaled of int
         | `stopped of int ]
   | `write_file_error of string * exn ]) Sequme_flow.t
(** Create a certificate for an entity of common name [name].
    If [name] is already used a new certificate is created for this
    “person”, if not a new entity is created.
*)

val crtkey_path : t -> name:string ->
  (string,
   [> `certificate_revoked of string * Time.t
   | `name_not_found of string ]) Result.t
(** Get the path to the 'crtkey' file of the server [name]. *)

val certificate_and_key_paths: t -> name:string ->
  (string * string,
   [> `certificate_revoked of string * Time.t
   | `name_not_found of string ]) Result.t
(** Get the paths to the 'crt' and 'key' files of the server [name]. *)

val certification_history: t -> name: string ->
  (string *
     [ `created of Core.Std.Time.t
     | `revoked of Core.Std.Time.t ] list) list option
(** Get the certification history of a given server. *)

val identify :
  t ->
  Ssl.certificate ->
  [> `entity_not_found of string
  | `ok of string
  | `revoked of string * Time.t
  | `wrong_subject_format of string ]
(** Find the name of the entity owning a given certificate. *)

val check_certificate : t -> Ssl.certificate ->
  ([> `not_found of string
   | `revoked of string * Core.Std.Time.t
   | `valid of string ],
   [> `wrong_subject_format of string ])
    Sequme_flow.t
(** A version of [identify] compatible with [Sequme_flow_net]. *)

val revoke : t -> name:string ->
  (unit,
   [> `name_not_found of string
   | `write_file_error of string * exn ]) Sequme_flow.t
(** Revoke [name]'s certificate. *)
