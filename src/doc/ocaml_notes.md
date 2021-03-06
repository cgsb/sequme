
# OCaml Links And Notes

## OCaml And Databases

### PG'OCaml

[PG'OCaml's website](http://pgocaml.forge.ocamlcore.org/):

-   [introductory blog
    post](http://unnali.com/2011/01/postgresql-and-pgocaml/)
-   Dario Teixeira's
    [tutorial](http://www.dse.nl/~dario/projects/pgoctut/)
-   Things like *LEFT OUTER JOIN* with *"nullable-results"* are in
    [BUGS.txt](https://github.com/angavrilov/pgocaml/blob/master-classic/BUGS.txt)
-   Postgres 9.1
    [manual](http://www.postgresql.org/docs/9.1/interactive/index.html)

Getting Postgres on a Debian-like system:

    apt-get install postgresql postgresql-client
    sudo su postgres
    createuser -s <username>

On CentOS it is more low-level (c.f. a few docs:
[1](http://www.cyberciti.biz/faq/howto-fedora-linux-install-postgresql-server/),
[2](http://www.cyberciti.biz/faq/howto-add-postgresql-user-account/)):

    yum install postgresql postgresql-server
    service postgresql initdb
    /etc/init.d/postgresql restart
    su - postgres
    psql template1

In *psql*:

    create user smondet;
    create database smondet;
    grant all privileges on database smondet to smondet;

### postgresql-ocaml

[On Markus Mottl's
website](http://www.ocaml.info/home/ocaml_sources.html#toc11); see also
[postgresql.mli](http://hg.ocaml.info/release/postgresql-ocaml/file/d90cc995646d/lib/postgresql.mli)

-   There are asynchronous functions but not at the Lwt level
-   Wrapper around the C libpq library
-   Used at Jane St (?)

### OCamlODBC

[Website](http://ocamlodbc.forge.ocamlcore.org/),
[ocamldoc](http://ocamlodbc.forge.ocamlcore.org/doc/Ocamlodbc.html),

-   Works with MySQL, PostgreSQL, UnixODBC, MS-Access
-   No asynchronous mode

### Ocsipersist

[Part of the
ocsigenserver](http://ocsigen.org/ocsigenserver/api/Ocsipersist), it
*lwt-ifies* [OCaml's `dbm`
library](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Dbm.html)
([darcs:src/extensions/ocsipersist-dbm](http://ocsigen.org/darcsweb/?r=ocsigenserver.dev;a=tree;f=/src/extensions/ocsipersist-dbm))
or [ocaml-sqlite3](http://www.ocaml.info/home/ocaml_sources.html)
([darcs:src/extensions/ocsipersist-sqlite](http://ocsigen.org/darcsweb/?r=ocsigenserver.dev;a=tree;f=/src/extensions/ocsipersist-sqlite)).

### ocaml-mysql

[ocaml-mysql.forge.ocamlcore.org](http://ocaml-mysql.forge.ocamlcore.org/)

### To Explore

-   [ocaml-leveldb](https://github.com/mfp/ocaml-leveldb): OCaml
    bindings for Google's LevelDB
-   [ocaml-cassandra](https://github.com/mfp/ocaml-cassandra): OCaml
    client library for the Apache Cassandra highly scalable distributed
    database
-   [Arakoon](http://arakoon.org/): distributed key-value store in
    OCaml. ([github](https://github.com/Incubaid/arakoon) and
    [bitbucket](https://bitbucket.org/despiegk/arakoon))
-   [hypertable
    bindings](https://forge.ocamlcore.org/projects/hypertable/) (c.f.
    [wikipedia](http://en.wikipedia.org/wiki/Hypertable))
-   [caml\_mongo](https://github.com/kiyoto/caml_mongo)
-   [Plasma \*](http://plasma.camlcity.org/plasma/index.html)

## Other Libs


### Core

Hitscore and hiscoreweb use [Jane St's
Core](http://ocaml.janestreet.com/?q=node/13)
([sources](https://bitbucket.org/yminsky/ocaml-core/overview)):

- [Index of the documentation](http://www.janestreet.com/ocaml/doc/core/index.html)
-   [Core.Std](http://www.janestreet.com/ocaml/doc/core/Std.html),
    [Time](http://www.janestreet.com/ocaml/doc/core/Time.html).{
    [Zone](http://www.janestreet.com/ocaml/doc/core/Std.Time.Zone.html),
    [Date](http://www.janestreet.com/ocaml/doc/core/Std.Time.Date.html)},
    [Arg](http://www.janestreet.com/ocaml/doc/core/Std.Arg.html),
    [List](http://www.janestreet.com/ocaml/doc/core/Core_list.html),
    [Array](http://www.janestreet.com/ocaml/doc/core/Core_array.html),
    [String](http://www.janestreet.com/ocaml/doc/core/Core_string.html) …
- [Sexplib](http://www.janestreet.com/ocaml/janestreet-ocamldocs/sexplib/index.html).
- [Sexp](http://www.janestreet.com/ocaml/janestreet-ocamldocs/sexplib/Sexp_intf.S.html)


### Batteries

Biocaml and Sequme still use
[Batteries](http://batteries.forge.ocamlcore.org/):

-   [ocamldoc](http://ocaml-batteries-team.github.com/batteries-included/hdoc/)
-   [source](https://github.com/ocaml-batteries-team/batteries-included)
-   [wiki](https://github.com/ocaml-batteries-team/batteries-included/wiki/)


### Ocsigen

C.f. [ocsigen.org](http://ocsigen.org).

-   [Mailing-list
    archives](https://sympa.mancoosi.univ-paris-diderot.fr/wws/arc/ocsigen)
    [/trac](https://ocsigen.org/trac/)
-   [Configuration file
    doc](http://ocsigen.org/ocsigenserver/manual/config)
-   Lwt: [manual](http://ocsigen.org/lwt/manual/)
    [sources](http://ocsigen.org/darcsweb/?r=lwt;a=tree)
-   Eliom [manual](http://ocsigen.org/eliom/manual/)
-   js\_of\_ocaml: [manual](http://ocsigen.org/js_of_ocaml/manual/),
    [o'closure manual](http://ocsigen.org/oclosure/dev/manual/)
    ([goog.\*](http://closure-library.googlecode.com/svn/docs/index.html)),
    [DOM-exceptions](http://reference.sitepoint.com/javascript/DOMException),
    [Goog.Ui.Menu
    demos](http://closure-library.googlecode.com/svn/trunk/closure/goog/demos/menu.html),
    [O'Closure
    examples](http://ocsigen.org/darcsweb/?r=oclosure;a=tree;f=/examples)
-   TyXML: [Html5](http://ocsigen.org/tyxml/api/HTML5.M),
    [Simplexmlparser](http://ocsigen.org/tyxml/api/Simplexmlparser)

### XMLM

The [Documentation](http://erratique.ch/software/xmlm/doc/Xmlm).

Create a DOM-style module:

```
module XML = struct
  include Xmlm
  type tree = E of tag * tree list | D of string
  let in_tree i = 
    let el tag childs = E (tag, childs)  in
    let data d = D d in
    input_doc_tree ~el ~data i
end
```

### OCaml-PAM

PAM: ([Pluggable authentication
modules](http://en.wikipedia.org/wiki/Pluggable_Authentication_Modules)).

[ocaml-pam](http://sharvil.nanavati.net/projects/ocamlpam/) 1.1 was
imported **and patched** in Hitscoreweb for testing:

-   [README](http://sharvil.nanavati.net/projects/ocamlpam/files/README.txt)
-   [ocsimore\_pam.ml](http://ocsigen.org/darcsweb/?r=ocsimore;a=plainblob;f=/src/user/ocsimore_pam.ml)
    (ocsigen.org)
-   *[pam\_start(3)](http://linux.die.net/man/3/pam_start)*,
    *[pam\_authenticate(3)](http://linux.die.net/man/3/pam_authenticate)*
