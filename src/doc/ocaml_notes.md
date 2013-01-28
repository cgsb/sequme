
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

