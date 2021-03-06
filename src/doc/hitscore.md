
# Hitscore

## Hitscore configuration file

The default path is: *\~/.config/hitscore/config.sexp*, but the
*hitscore* and *hitscoreweb* commands can be called with a different
path (*':'* is the separator), e.g.:
`hitscore myfile.sexp:specialprofile <command> ...`

This one defines one *profile* “*test*”, Hitscore will in that case use
the local/default database of the user:

```
(profile test
 (root "/home/sm4431/tmp/gencore_extract/gencore/"
  (writers sm4431 root)
  (group admin))
 (raw "/home/sm4431/tmp/gencore_extract/gencore-raw"
  (hiseq "HiSeq"))
 (work "/tmp/zz_hs_work/")
 )
```

```
(profile dev
         (root "/scratch/sm4431/gencore-dev/"
          (writers sm4431 aa144)
          (group cgsb))
 	 (raw "/data/cgsb/gencore-raw"
	  (hiseq "HiSeq"))
         (work "/scratch/sm4431/gencorework-dev/")
          (db
           (host "pg.bio.nyu.edu")
           (port 5432)
           (database "hitscore-dev")
           (username "hitscore-dev")
           (password "very long secret")))

(profile production
         (root "/data/cgsb/gencore/"
           (writers sm4431 aa144)
           (group cgsb))
 	 (raw "/data/cgsb/gencore-raw"
	  (hiseq "HiSeq"))
         (work "/scratch/sm4431/gencorework-dev/")
         (work "/scratch/sm4431/gencorework/")
         (db
          (host "pg.bio.nyu.edu")
          (port 5432)
          (database "hitscore")
          (username "hitscore")
          (password "very long secret")))
```

## Quick HOWTOs

### Layout Changes

After editing `data/hitscore_layout`:

- update `./src/migrator/mk` (add the future version if not already
  there, remove useless old versions).
- commit (or stash?) everything so that the Git index is clean. And a
  *tag* for the development version e.g `v3.4-dev`.
- run `./src/migrator/mk update` (this will use Git to retrieve old
  tagged versions)
- now you can do the layout updates in `./src/migrator/migrator.ml`
- compile them with `./src/migrator/mk`

Often the layout updates are simply adding empty records or fields, e.g.:

```
dump_v06
|! add_empty "record_fastx_results"
|! add_empty "function_fastx_quality_stats_of_unaligned"
|! parse
```

The migrator is easy to test with a dump of the layout:

```
 $ ./_migrator/migrator v06-v07 some_dump_06.sexp out07.sexp
```

The migrator already checks that the resulting `out07.sexp` has the
right format, but doing a full load with an empty database will allow more
checks (unique pointers, etc.):

```
 $ hitscore test load-file out07.sexp
 $ hitscore test verify-layout
```

### Importing Submission Sheets

Export the google-doc to CSV.

Test the import (read the output, check everything):

     $ hitscore <profile> pss <phix_spec> <file_csv>

Do the real one (check the run-plan for PhiX spike-in):

     $ hitscore production pss -wet-run A:1,B:2 pss.csv

`A:1,B:2` means that the pool A has 1% PhiX and the pool B 2%. If no
pool has PhiX, the user has to specify 'No\_PhiX' (case insensitive).

Once all the pools are in, check their IDs:

     $ hitscore production Q orphan-lanes

Register the new flowcell:

     $ hitscore production help register-flowcell
    Usage: hitscore <profile> register-flowcell <flowcell-name> <L1> <L2> .. <L8>
    where Lx is 'PhiX', 'Empty', or an orphan lane's id.

     $ hitscore production register-flowcell SERIALCXX PhiX 42 43 Empty 56 57 58 59

Once we have two flowcells:

     $ hitscore production help register-hiseq-run
    Usage: hitscore <profile> register-hiseq-run <date> <fcidA> <fcidB> [<note>]
      where fcid is a database id, a proper FCID, or '_'

### Hiseq-raw Directories

As simple as:

     $ hitscore production help register-hiseq-raw
    usage: hitscore <profile> register-hiseq-raw [-host <host-addr>] <absolute-path>
       (default host being bowery.es.its.nyu.edu)

     $ hitscore production register-hiseq-raw /absolute/path/to/111004_SN911_0079_BD053MACXX/


### Demultiplexing

Choose and Hiseq-raw directory (e.g. from the output of
register-hiseq-raw, the
[/layout](https://gencore.bio.nyu.edu/layout?type=record_hiseq_raw&action=view)
service, or `select * in hiseq_raw`, …).

     $ hitscore production bcl-to-fastq start  <nb>

See

     $ hitscore production bcl-to-fastq start -help

for options (sample-sheet kind, tiles, wall-hours, etc.).


### Generating FASTX Quality Statistics

The command `fastx-quality-stats` (a.k.a. `fxqs`), has the same kind of
interface as `b2f`.

      $ hitscore dev help fxqs
    Usage: hitscore <profile> fxqs <command> <args>
    Where the commands are:
      * start: start the function (try "-help").
      * register-success <id>.
      * register-failure <id> [<reason-log>].
      * status <id> : Get the current status of an evaluation.
      * fix-status <id> : Get the status and fix it if possible
      * kill <id>.

Right now, its input is the ID of a given *bcl-to-fastq* run For
example, after the *bcl-to-fastq* 42 has finished successfully:

    hitscore production fxqs start -from-b2f 42


### Delivery

Use the *custom-query* “`deliveries`” to know what is available to
deliver, then the command `deliver` to do the actual links with ACLs.

Example delivering C0HYHACXX:

     $ hitscore production Q deliveries  C0HYHACXX
    Found 4 invoices:
      10 (to Rockman)
      11 (to Gunsalus)
      12 (to Carlton)
      13 (to Purugganan)
    Found 1 bcl-to-fastqs:
      34 (tiles: N/A, result: 111/B2F_C0HYHACXX_2012-03-02_11-11-37.744094_S10_H15_M1_1.8.2_sm4431)

And then for each one needed:

    $ hitscore production deliver 34 10 /data/cgsb/gencore/out 2012-02-06


### Deleting Intensities

for now the deletion is manual, but one can register a successful one:

     $ hitscore production delete-intensities register <nb>

where `<bn>` is an Hiseq-raw directory `g_id`.

### Website Deployment

Go to WSO6.

Pull, compile, and install hitscore and hitscoreweb (and any other
dependency …).

    hitscoreweb <target_config> rpm -rpm-release 8 \
      -ssl certificate.crt unencrypted-key.key \
      -ssl-dir ssl-directory \
      -pam pam-service

The release number should be increased every time for a given hitscore
version (at least if we want to be RPM-clean).

RPMs go to `in /tmp/hitscorerpmbuild/RPMS/x86_64`, send the latest one
to `WSO1`/`WSO5`, and:

As root:

     # service hitscoreweb stop
     # killall hitscoreserver   #sometimes needed (TODO: fix)
     # rpm -e hitscoreweb
     # rpm -i /tmp/hitscorerpmbuild/RPMS/x86_64/hitscoreweb-0.6-3.x86_64.rpm
     # service hitscoreweb start

One may use something like this after stopping the service:

     # while true; do echo "Gencore stopped for Maintenance, come back in a few minutes" | nc -l 80 ; done


### Migration Between Versions

Follow the following steps to do in order to switch to a new version
`V.W`:

-   (if not already done) switch the version to `V.W`
    -   In the `_oasis` file (requires an `ocaml setup.ml -distclean`,
        and an **`oasis setup`**)
    -   in the migrator: `mk` script (with the *future* `vV.W` git tag)

-   merge to `master`
-   `git tag -a vV.W -m '...'`
-   With the previous version of `hitscore`:
    -   `hitscore production dump-to-file hitscore_dump.sexp`

- Put the website in maintenance mode:
    -  `echo "maintenance:on" >> /var/hitscoreweb/ocsigen_command`

-   Compile `hitscore` and the migrator with v. `V.W`, and:
    -   `_migrator/migrator vPrev-vV.W hitscore_dump.sexp hitscore_to_load.sexp`
    -   `hitscore production wipe-out-database I am sure` *(or re-init
        the DB in some other way)*
    -   `hitscore production check-db`
    -   `hitscore production load-file hitscore_to_load.sexp`
