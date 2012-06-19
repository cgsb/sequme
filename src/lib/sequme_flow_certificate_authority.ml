open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

let _master_filename = "sequme_flow_certificate_authority_master.sexp"

type certificate_status = [`created of Time.t | `revoked of Time.t] with sexp

type certificate = {
  cert_prefix: string; (* The 'name' for the .crt, .key, etc. files *)
  cert_cn: string; (* The CN in the cert, the actual format is fixed:
                      {Server,Client}<index-nb> name
                      => the name is recomputable from the CN *)
  mutable cert_status: certificate_status;
  mutable cert_history: certificate_status list;
} with sexp
  
type certification = {
  name: string;
  mutable kind: [`server | `client ];
  mutable certificate: certificate;
  mutable history : certificate list;
} with sexp
  
type t = {

  openssl_command : string;
  path: string;

  (* DN values *)
  dn_country: string;
  dn_province: string;
  dn_city: string;
  dn_org: string;
  dn_orgunit: string;
  dn_email: string;

  rsa_key_size: int;

  default_validity: int;

  ca_filename_prefix: string;
  ca_cn: string;

  mutable entities: certification String.Map.t;
  mutable entity_index : int;
    
} with sexp

let create
    ?(openssl_command="openssl")
    ?(dn_country="US")
    ?(dn_province="TX")
    ?(dn_city="Austin")
    ?(dn_org="Primus Ltd.")
    ?(dn_orgunit="")
    ?(dn_email="")
    ?(rsa_key_size = 4096)
    ?(default_validity = 3650)

    ?(ca_filename_prefix="sequme_flow_certificate_authority")
    ?(ca_cn="Sequme_CA")

    path =
  (* create necessary files *)
  {
    openssl_command;
    dn_country;
    dn_province;
    dn_city;
    dn_org;
    dn_orgunit;
    dn_email;
    rsa_key_size;
    default_validity;
    ca_filename_prefix;
    ca_cn;
    entities = String.Map.empty;
    entity_index = 0;
    path
  }

let openssl_config_file_string t =
  String.concat ~sep:"\n" [
    "
#
# OpenSSL configuration for ssl-util.
# Based on sample configuration shipped with OpenVPN.
#
";
    sprintf "RANDFILE  = %s/_rand_file" t.path;
    "
openssl_conf           = openssl_init
[ openssl_init ]
oid_section            = new_oids
engines                = engine_section
[ new_oids ]
[ engine_section ]
[ ca ]
default_ca             = CA_default
[ CA_default ]";
    sprintf "dir = %s" t.path;
    "
certs                  = $dir
crl_dir                = $dir
database               = $dir/index.txt
new_certs_dir          = $dir";
    sprintf "certificate = $dir/%s.crt" t.ca_filename_prefix;
    "
serial                 = $dir/serial
crl                    = $dir/crl.pem";
    sprintf "private_key            = $dir/%s.key" t.ca_filename_prefix;
    "
RANDFILE               = $dir/.rand
x509_extensions        = usr_cert";
    sprintf "default_days = %d" t.default_validity;
    sprintf "default_crl_days = %d" t.default_validity;
    "
# IMPORTANT: The next must no longer be md5, if used with
# Debian's OpenLDAP package being compiled against libgnutls.
default_md             = sha1
preserve               = no
policy                 = policy_match

[ policy_match ]

countryName            = match
stateOrProvinceName    = match
organizationName       = match
organizationalUnitName = optional
commonName             = supplied
emailAddress           = optional

[ policy_anything ]

countryName            = optional
stateOrProvinceName    = optional
localityName           = optional
organizationName       = optional
organizationalUnitName = optional
commonName             = supplied
emailAddress           = optional

[ req ]
";
    sprintf "default_bits = %d" t.rsa_key_size;
    "
default_keyfile        = privkey.pem
distinguished_name     = req_distinguished_name
attributes             = req_attributes
x509_extensions        = v3_ca
string_mask            = nombstr

[ req_distinguished_name ]

countryName            = Country Name (2 letter code)
";
    sprintf "countryName_default    = %S" t.dn_country;
    "
countryName_min        = 2
countryName_max        = 2

stateOrProvinceName    = State or Province Name (full name)
";
    sprintf "stateOrProvinceName_default = %S" t.dn_province;
    "localityName           = Locality Name (eg, city)";
    sprintf "localityName_default   = %S" t.dn_city;
    sprintf "0.organizationName = Organization Name (eg, company)";
    sprintf "0.organizationName_default = %S" t.dn_org;
    "organizationalUnitName = Organizational Unit Name (eg, section)";
    sprintf "organizationalUnitName_default = %S" t.dn_orgunit;
    "commonName = Common Name (eg, your name or your server\'s hostname)";
    sprintf "commonName_default = $ENV::CN";
    "
commonName_max         = 64

emailAddress           = Email Address" ;
    sprintf "emailAddress_default   = %S" t.dn_email;
    "
emailAddress_max       = 40

[ req_attributes ]

challengePassword      = A challenge password
challengePassword_min  = 4
challengePassword_max  = 20
unstructuredName       = An optional company name

[ usr_cert ]

basicConstraints       = CA:FALSE
nsComment              = \"OpenSSL Generated Certificate\"
subjectKeyIdentifier   = hash
authorityKeyIdentifier = keyid,issuer:always
extendedKeyUsage       = clientAuth
keyUsage               = digitalSignature

[ server ]

basicConstraints       = CA:FALSE
nsCertType             = server
nsComment              = \"OpenSSL Generated Server Certificate\"
subjectKeyIdentifier   = hash
authorityKeyIdentifier = keyid,issuer:always
extendedKeyUsage       = serverAuth
keyUsage               = digitalSignature, keyEncipherment

[ v3_req ]

basicConstraints       = CA:FALSE
keyUsage               = nonRepudiation,digitalSignature,keyEncipherment

[ v3_ca ]

subjectKeyIdentifier   = hash
authorityKeyIdentifier = keyid:always,issuer:always
basicConstraints       = CA:true

[ crl_ext ]

authorityKeyIdentifier = keyid:always,issuer:always


" ]

let openssl_config_path t = Filename.concat t.path "openssl.cnf"

let ca_key_path t = Filename.concat t.path (sprintf "%s.key" t.ca_filename_prefix)
let ca_crt_path t = Filename.concat t.path (sprintf "%s.crt" t.ca_filename_prefix)

let ca_certificate_path t = ca_crt_path t
    
let master_path t = Filename.concat t.path _master_filename
  
let cmd ?(verbose=false) fmt =
  ksprintf (fun s ->
    get_system_command_output s
    >>= fun (stdout, stderr) ->
    if verbose then
      bind_on_error
        (catch_io
           (Lwt_io.printf "<CMD %s>\n<out>%s</out><err>%s</err></CMD>" s stdout)
           stderr)
        (fun e -> return ())
    else
      return ())
    fmt

let save t =
  write_file (master_path t) ~content:(sexp_of_t t |! Sexp.to_string_hum)
  
  
let establish t =
  cmd "mkdir -m 700 -p %S" t.path >>= fun () ->
  let content = openssl_config_file_string t in
  write_file (openssl_config_path t) ~content >>= fun () ->
  cmd "export CN=%S && %s req -batch -config %s -nodes -new -x509 -days %d \
       -keyout %s -out %s && touch %s && echo 01 > %s"
    t.ca_cn t.openssl_command (openssl_config_path t) t.default_validity
    (ca_key_path t) (ca_crt_path t)
    (Filename.concat t.path "index.txt")
    (Filename.concat t.path "serial")
  >>= fun () ->
  save t >>= fun () ->
  return ()

let load path =
  let database_file_path = Filename.concat path _master_filename in
  read_file database_file_path >>= fun str ->
  wrap ~on_exn:(fun e -> `parse_config_error e)
    ~f:(fun s -> Sexp.of_string str |! t_of_sexp) str

let escape_for_filename =
  String.map ~f:(function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' as c -> c
  | _ -> '_')
    
let make_certificate ?verbose t ~kind ~name =
  let kindstr = match kind with `server -> "server" | `client -> "client" in
  let cert_prefix =
    sprintf "%08d-%s__%s" t.entity_index kindstr (escape_for_filename name) in
  let cert_prefix_path = Filename.concat t.path cert_prefix in
  let cert_cn = sprintf "%08d-%s %s" t.entity_index kindstr name in
  t.entity_index <- t.entity_index + 1;
  let server_extensions =
    match kind with `server -> "-extensions server" | `client -> "" in
  cmd ?verbose "%s" (String.concat ~sep:" " [
    sprintf "export CN=%S &&" cert_cn;
    sprintf "%s req -batch" t.openssl_command;
    sprintf "-config %S" (openssl_config_path t);
    "-nodes -new";
    server_extensions;
    sprintf "-keyout %s.key" cert_prefix_path;
    sprintf "-out %s.csr" cert_prefix_path;
    "&&";
    sprintf "%s ca -batch" t.openssl_command;
    sprintf "-config %S" (openssl_config_path t);
    server_extensions;
    sprintf "-out %s.crt" cert_prefix_path;
    sprintf "-in %s.csr" cert_prefix_path;
    "&&";
    sprintf "cat %s.crt %s.key > %s.crtkey"
      cert_prefix_path cert_prefix_path cert_prefix_path;
  ])
  >>= fun () ->
  let new_cert =
    { cert_prefix; cert_cn; cert_history = [];
      cert_status = `created Time.(now ()) } in
  begin match String.Map.find t.entities name with
  | None ->
    let data = { name ; kind = `server; certificate = new_cert;
                 history = [] } in
    t.entities <- String.Map.add t.entities ~key:name ~data;
  | Some certification ->
    certification.kind <- `server;
    certification.history <- certification.certificate :: certification.history;
    certification.certificate <- new_cert;
  end;
  save t

(*
  CN="${1}" openssl req -batch -config "${SSLCONF}" \
    -nodes -new -extensions server \
    -keyout "${CERTSDIR}/${1}.key" -out "${CERTSDIR}/${1}.csr" && \
    openssl ca -batch -config "${SSLCONF}" -extensions server \
    -out "${CERTSDIR}/${1}.crt" -in "${CERTSDIR}/${1}.csr" && \
    cat "${CERTSDIR}/${1}.crt" "${CERTSDIR}/${1}.key" > "${CERTSDIR}/${1}.crtkey"
*)

let find_certificate t ~name =
  let open Option in
  String.Map.find t.entities name >>= fun certification ->
  return certification.certificate
    
let get_certificate_prefix t ~name =
  begin match find_certificate t ~name with
  | None -> Error (`name_not_found name)
  | Some {cert_status = `created _; cert_prefix } ->
    Ok cert_prefix
  | Some {cert_status = `revoked t} ->
    Error (`certificate_revoked (name, t))
  end
    
let crtkey_path t ~name =
  let open Result in
  get_certificate_prefix t ~name >>= fun prefix ->
  return (Filename.concat t.path (sprintf "%s.crtkey" prefix))
    
let certificate_and_key_paths t ~name =
  let open Result in
  get_certificate_prefix t ~name >>= fun prefix ->
  return (Filename.concat t.path (sprintf "%s.crt" prefix),
          Filename.concat t.path (sprintf "%s.key" prefix))

let certification_history t ~name =
  let open Option in
  String.Map.find t.entities name >>= fun certification ->
  return (List.map (certification.certificate :: certification.history)
            (fun certificate ->
              (certificate.cert_prefix,
               certificate.cert_status :: certificate.cert_history)))

let login_of_subject subj =
  List.find_map (String.split subj ~on:'/') (function
  | s when String.prefix s 3 = "CN=" ->
    begin match String.lsplit2 ~on:' ' (String.drop_prefix s 3) with
    | Some (_, login) -> Some login
    | _ -> None
    end
  | _ -> None)
    
let identify t tls_cert =
  let subj = Ssl.get_subject tls_cert in
  begin match login_of_subject subj with
  | Some name ->
    begin match find_certificate t name with
    | Some {cert_status = `created _ } -> `ok name
    | Some {cert_status = `revoked t } -> `revoked (name, t)
    | None -> `entity_not_found name
    end
  | None -> `wrong_subject_format subj
  end

let check_certificate t tls_cert =
  match identify t tls_cert with
  | `ok n -> return (`valid n)
  | `revoked (n, t) -> return (`revoked (n, t))
  | `entity_not_found n  -> return (`not_found n)
  | `wrong_subject_format subj as e -> error e

let revoke t ~name =
  begin match find_certificate t name with
  | None -> error (`name_not_found name)
  | Some cert ->
    cert.cert_status <- (`revoked Time.(now ()));
    save t
  end

  
