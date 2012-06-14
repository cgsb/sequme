open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

let _master_filename = "sequme_flow_certificate_authority_master.sexp"

type certificate = {
  cert_prefix: string;
  mutable cert_history:
    [`created of Time.t | `revoked of Time.t] list;
} with sexp
  
type certification = {
  name: string;
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

  mutable servers: certification String.Map.t;
  mutable server_index : int;
    
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
    servers = String.Map.empty;
    server_index = 0;
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
    
let master_path t = Filename.concat t.path _master_filename
  
let cmd fmt = ksprintf system_command fmt

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
    
let make_server_certificate t ~name =
  let cert_prefix =
    sprintf "Server_%s_%08d" (escape_for_filename name) t.server_index in
  t.server_index <- t.server_index + 1;
  let cert_prefix_path = Filename.concat t.path cert_prefix in
  system_command (String.concat ~sep:" " [
    sprintf "export CN=%S &&" cert_prefix;
    sprintf "%s req -batch" t.openssl_command;
    sprintf "-config %S" (openssl_config_path t);
    "-nodes -new -extensions server";
    sprintf "-keyout %s.key" cert_prefix_path;
    sprintf "-out %s.csr" cert_prefix_path;
    "&&";
    sprintf "%s ca -batch" t.openssl_command;
    sprintf "-config %S" (openssl_config_path t);
    "-extensions server";
    sprintf "-out %s.crt" cert_prefix_path;
    sprintf "-in %s.csr" cert_prefix_path;
    "&&";
    sprintf "cat %s.crt %s.key > %s.crtkey"
      cert_prefix_path cert_prefix_path cert_prefix_path;
  ])
  >>= fun () ->
  let new_cert = { cert_prefix; cert_history = [`created Time.(now ())] } in
  begin match String.Map.find t.servers name with
  | None ->
    let data = { name ; history = [new_cert] } in
    t.servers <- String.Map.add t.servers ~key:name ~data;
  | Some certification ->
    certification.history <- new_cert :: certification.history;
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

let server_valid_certificate t ~name =
  let open Option in
  String.Map.find t.servers name >>= fun certification ->
  List.hd certification.history >>= fun current_certificate ->
  List.hd current_certificate.cert_history >>= fun status ->
  begin match status with
  | `created _ -> return (current_certificate)
  | `revoked _ -> None
  end
  
let server_crtkey_path t ~name =
  let open Option in
  server_valid_certificate t ~name >>= fun current_certificate ->
  return (Filename.concat t.path
            (sprintf "%s.crtkey" current_certificate.cert_prefix))
    
let server_certificate_and_key_paths t ~name =
  let open Option in
  server_valid_certificate t ~name >>= fun current_certificate ->
  return (Filename.concat t.path
            (sprintf "%s.crt" current_certificate.cert_prefix),
          Filename.concat t.path
            (sprintf "%s.key" current_certificate.cert_prefix))
