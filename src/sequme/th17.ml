open Batteries_uni;; open Printf

exception Error of string

module Lane = struct
  let id_of_sl_id dbh sl_id = match PGSQL(dbh)
      "SELECT th17_lane.id FROM th17_sample,th17_lane
       WHERE th17_sample.id = th17_lane.sample_id
       AND th17_sample.sl_id = $sl_id"
    with
      | [] -> Error (sprintf "no lane for sample with SL ID = %s" sl_id) |> raise
      | x::[] -> x
      | _ -> Error (sprintf "multiple lanes for sample with SL ID = %s" sl_id) |> raise

  let sample_id_of_id dbh id =
    match PGSQL(dbh) "SELECT sample_id FROM th17_lane WHERE id = $id" with
      | [] -> Error (sprintf "unknown lane ID %ld" id) |> raise
      | x::[] -> x
      | _ -> assert false

  let fastq_file_path_of_id sequme_root id =
    let dir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "lane"; Int32.to_string id]
    in

    let files = dir |> Sys.files_of
      |> Enum.filter (flip Filename.check_suffix ".fastq")
      |> List.of_enum
    in

    match files with
      | [] -> Error (sprintf "fastq file not found for lane %ld" id) |> raise
      | x::[] -> Filename.concat dir x
      | _ -> Error (sprintf "multiple fastq files found for lane %ld" id) |> raise

end

module Bowtie = struct
  let run conf dbh sl_id =
    let exec = "/share/apps/bowtie/0.12.7/gnu/bowtie" in
    let version = "0.12.7" in
    let index_base = "mm9" in
    let k = 1l in
    let best = true in
    let sam = true in
    let num_threads = 3l in
    let lane_id = Lane.id_of_sl_id dbh sl_id in
    let started = Util.now() in
    let status = "in_progress" in
    
    let sequme_root = Map.StringMap.find "sequme_root" conf in
    let outdir = Util.temp_dir
      ~parent_dir:(Filename.concat sequme_root "tmp")
      ~perm:0o755 (sl_id ^ "_") "_th17_bowtie"
    in
    let hit_file = Filename.concat outdir (sl_id ^ ".sam") in

    let note = sprintf "temporary output directory: %s" outdir in

    PGSQL(dbh)
      "INSERT INTO th17_bowtie
       (exec_path,version,index_base,k,best,sam,num_threads,lane_id,started,status,note)
       VALUES
       ($exec,$version,$index_base,$k,$best,$sam,$num_threads,$lane_id,$started,$status,$note)"
    ;

    let bowtie_id = PGOCaml.serial4 dbh "th17_bowtie_id_seq" in

    let cmd = Bowtie.make_cmd ~exec
      ~ebwt:(Bowtie.path_of_index conf index_base)
      ~k:(Int32.to_int k)
      ~best ~sam
      ~threads:(Int32.to_int num_threads)
      ~hit:hit_file
      ~reads:(Lane.fastq_file_path_of_id sequme_root lane_id)
    in

    let pbs_outdir = Filename.concat outdir "pbs_out" in
    let pbs_stdout_file = Filename.concat pbs_outdir "stdout.txt" in
    let pbs_stderr_file = Filename.concat pbs_outdir "stderr.txt" in
    let pbs_script_file = Filename.concat pbs_outdir "script.pbs" in
    let qsub_out_file = Filename.concat pbs_outdir "qsub_out.txt" in

    let script = Pbs.make_script
      (* ~mail_options:[Pbs.JobAborted; Pbs.JobBegun; Pbs.JobEnded] *)
      (* ~user_list:["ashish.agarwal@nyu.edu"] *)
      ~resource_list:"nodes=1:ppn=8,mem=14gb"
      (* ~priority:(-1024) *)
      ~job_name:(sprintf "bowtie_%s" sl_id)
      ~stdout_path:pbs_stdout_file
      ~stderr_path:pbs_stderr_file
      ~export_qsub_env:true
      ~rerunable:false
      [
        "";
        sprintf "echo %ld > %s" bowtie_id (Filename.concat outdir "th17_bowtie_id");
        "";
        Bowtie.cmd_to_string cmd
      ]
    in

    Unix.mkdir pbs_outdir 0o755;
    Pbs.script_to_file script ~perm:(File.unix_perm 0o644) pbs_script_file;
    let cmd = sprintf "qsub %s > %s 2>&1" pbs_script_file qsub_out_file in
    print_endline cmd;
    match Sys.command cmd with
      | 0 -> ()
      | x -> eprintf "qsub returned exit code %d" x


  let any_id_of_sl_id dbh sl_id =
    match PGSQL(dbh)
      "SELECT bowtie.id
       FROM th17_sample as sample, th17_lane as lane, th17_bowtie as bowtie
       WHERE bowtie.lane_id = lane.id
       AND lane.sample_id = sample.id
       AND sample.sl_id = $sl_id"
    with
      | [] -> Error (sprintf "bowtie run for sample with SL ID %s not found" sl_id) |> raise
      | x::_ -> x


  let delete conf dbh id =
    match PGSQL(dbh) "SELECT id FROM th17_bowtie WHERE id=$id" with
      | [] -> Error (sprintf "bowtie run with ID %ld unknown" id) |> raise
      | _::_::_ -> assert false
      | _::[] ->
          PGSQL(dbh) "DELETE FROM th17_bowtie WHERE id=$id";
          let dir = List.reduce Filename.concat
            [Map.StringMap.find "sequme" conf; "db"; "th17"; "bowtie"; Int32.to_string id]
          in
          sprintf "rm -rf %s" dir |> Sys.command |> ignore

  let sam_file_path_of_id sequme_root id =
    let dir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "bowtie"; Int32.to_string id]
    in

    let files = dir |> Sys.files_of
      |> Enum.filter (flip Filename.check_suffix ".sam")
      |> List.of_enum
    in

    match files with
      | [] -> Error (sprintf "SAM file not found for bowtie run %ld" id) |> raise
      | x::[] -> Filename.concat dir x
      | _ -> Error (sprintf "multiple SAM files found for bowtie run %ld" id) |> raise

end

module Macs = struct
  let run conf dbh treatment control =
    let sequme_root = Map.StringMap.find "sequme_root" conf in

    let exec = "/home/aa144/local/python/bin/macs14" in
    let version = "macs14 1.4.0rc2 20110214 (Valentine)" in
    let format = "sam" in
    let pvalue = "1e-10" in
    let mfold_low = 15l in
    let mfold_high = 30l in
    let tsize = 36l in
    let gsize = "mm" in
    let bw = 200l in

    let treatment_bowtie_id = Bowtie.any_id_of_sl_id dbh treatment in
    let treatment_sam_file = Bowtie.sam_file_path_of_id sequme_root treatment_bowtie_id in
    let control_bowtie_id = Bowtie.any_id_of_sl_id dbh control in
    let control_sam_file = Bowtie.sam_file_path_of_id sequme_root control_bowtie_id in

    let note = "" in
    let status = "in_progress" in
    let started = Util.now() in

    PGSQL(dbh)
      "INSERT INTO th17_macs
       (exec_path,version,started,status,note,format,pvalue,
        mfold_high,mfold_low,tsize,gsize,bw,control_id,treatment_id)
       VALUES
       ($exec,$version,$started,$status,$note,$format,$pvalue,
        $mfold_high,$mfold_low,$tsize,$gsize,$bw,$control_bowtie_id,$treatment_bowtie_id)"
    ;
    let macs_id = PGOCaml.serial4 dbh "th17_macs_id_seq" in

    let outdir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "macs"; Int32.to_string macs_id] in

    Unix.mkdir outdir 0o755;

    let macs_cmd = Macs.make_cmd ~exec
      ~format ~pvalue ~mfold:(mfold_low,mfold_high)
      ~tsize ~gsize ~bw
      ~control:control_sam_file ~treatment:treatment_sam_file in

    let macs_outdir = Filename.concat outdir "macs_out" in
    Unix.mkdir macs_outdir 0o755;

    let cmds = [
      sprintf "cd %s" macs_outdir;
      "";
      Macs.cmd_to_string macs_cmd;
    ] in

    let job_name = sprintf "%s_%s" treatment control |> flip String.left 15 in
    let pbs_outdir = Filename.concat outdir "pbs_out" in
    Pbs.make_and_run ~job_name pbs_outdir cmds


  let delete conf dbh id =
    match PGSQL(dbh) "SELECT id FROM th17_macs WHERE id=$id" with
      | [] -> Error (sprintf "MACS run %ld unknown" id) |> raise
      | _::_::_ -> assert false
      | _::[] ->
          PGSQL(dbh) "DELETE FROM th17_macs WHERE id=$id";
          let dir = List.reduce Filename.concat
            [Map.StringMap.find "sequme" conf; "db"; "th17"; "macs"; Int32.to_string id]
          in
          sprintf "rm -rf %s" dir |> Sys.command |> ignore

end
