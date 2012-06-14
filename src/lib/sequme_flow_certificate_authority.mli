(** Certificate authority (for now, wrap [openssl]). *)

type t


val create :
  ?openssl_command:string ->
  ?dn_country:string ->
  ?dn_province:string ->
  ?dn_city:string ->
  ?dn_org:string ->
  ?dn_orgunit:string ->
  ?dn_cn:string ->
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
