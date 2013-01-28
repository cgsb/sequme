
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

