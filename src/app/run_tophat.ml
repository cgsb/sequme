open Batteries_uni;; open Printf;; open Sequme

let run conf outdir dataset =
  let cmd = TopHat.make_cmd
    ~min_anchor_length:10
    ~solexa1_3_quals:true
    ~num_threads:8
    ~max_multihits:20
    ~gtf:(Cache.path_of_short_name conf "refGeneAnnot.gtf")
    ~no_novel_juncs:true
    ~output_dir:(Filename.concat outdir "tophat_out")
    (Bowtie.path_of_index conf "mm9")
    [Sequme.HudsonAlpha.fastq_path_of_libid conf dataset]
    []
  in
  
  let pbs_outdir = Filename.concat outdir "pbs_out" in
  let script = Pbs.make_script
    ~mail_options:[Pbs.JobAborted; Pbs.JobBegun; Pbs.JobEnded]
    ~user_list:["ashish.agarwal@nyu.edu"]
    ~resource_list:"nodes=1:ppn=1"
    ~job_name:(sprintf "tophat_%s" dataset)
    ~stdout_path:(Filename.concat pbs_outdir "stdout.txt")
    ~stderr_path:(Filename.concat pbs_outdir "stderr.txt")
    ~export_qsub_env:true
    ~rerunable:false
    [TopHat.cmd_to_string cmd]
  in
  let script_file = Filename.concat pbs_outdir "script.pbs" in
  
  Unix.mkdir outdir 0o755;
  Unix.mkdir pbs_outdir 0o755;
  Pbs.script_to_file script ~perm:(File.unix_perm 0o644) script_file;
  sprintf "qsub %s" script_file |> Sys.command |> ignore

;;
let conf = Conf.read "/data/sequme" in
let outdir = "/data/sequme/log/2011-01-14_tophat/" in
Unix.mkdir outdir 0o755;
for i = 1 to Array.length Sys.argv - 1 do
  run conf (Filename.concat outdir Sys.argv.(i)) Sys.argv.(i)
done;
