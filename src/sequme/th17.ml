open Batteries_uni;; open Printf

exception Error of string

type dbh = (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t
type timestamptz = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t

module CachedFile = struct

  let id_of_name dbh name =
    match PGSQL(dbh) "SELECT id FROM th17_cachedfile WHERE name=$name" with
      | x::[] -> x
      | _ -> assert false

  let name_of_id dbh id =
    match PGSQL(dbh) "SELECT name FROM th17_cachedfile WHERE id=$id" with
      | x::[] -> x
      | _ -> assert false

  let path_of_id sequme_root dbh id =
    List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "cachedfile"; name_of_id dbh id]

end

module Sample = struct

  type t = {
    id : int32;
    owner_id : int32;
    sample_made_by_id : int32;
    library_made_by_id : int32;
    created : timestamptz;
    last_modified : timestamptz;
    name : string;
    sl_id : string;
    note : string;
    exp_type : string;
    cell_type : string;
    condition : string;
    factor_ChIP : string;
    antibody_ChIP : string;
    target_ChIP : string
  }

  let all_ids dbh = PGSQL(dbh) "SELECT id FROM th17_sample"

  let of_id dbh id =
    match PGSQL(dbh)
      "SELECT id,owner_id,sample_made_by_id,library_made_by_id,
              created,last_modified,name,sl_id,note,exp_type,
              cell_type,condition,\"factor_ChIP\",\"antibody_ChIP\",\"target_ChIP\"
       FROM th17_sample WHERE id=$id"
    with
      | (id,owner_id,sample_made_by_id,library_made_by_id,
        created,last_modified,name,sl_id,note,exp_type,
        cell_type,condition,factor_ChIP,antibody_ChIP,target_ChIP)::[]
        -> Some {
          id;owner_id;sample_made_by_id;library_made_by_id;
          created;last_modified;name;sl_id;note;exp_type;
          cell_type;condition;factor_ChIP;antibody_ChIP;target_ChIP
        }
      | [] -> None
      | _ -> assert false

  let id_to_sl_id dbh id =
    match PGSQL(dbh) "SELECT sl_id FROM th17_sample WHERE id=$id" with
      | x::[] -> x
      | _ -> assert false

  let sl_id_to_id dbh sl_id =
    match PGSQL(dbh) "SELECT id FROM th17_sample WHERE sl_id=$sl_id" with
      | x::[] -> x
      | _ -> assert false

end

module Lane = struct

  type t = {
    id : int32;
    number : int;
    illuminarun_id : int32;
    sample : Sample.t
  }

  let all_ids dbh = PGSQL(dbh) "SELECT id FROM th17_lane"

  let of_id dbh id =
    PGOCaml.begin_work dbh;
    let ans =
      match PGSQL(dbh)
        "SELECT id,number,illuminarun_id,sample_id
         FROM th17_lane WHERE id=$id"
      with
        | (id,number,illuminarun_id,sample_id)::[] ->
            Some {
              id; number; illuminarun_id;
              sample = Sample.of_id dbh sample_id |> Option.get
            }
        | [] -> None
        | _ -> assert false
    in
    PGOCaml.commit dbh;
    ans

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

  let not_downloaded_list password dbh =
    let open HudsonAlpha in
    let module Set = Set.StringSet in
    let available =
      get_status_html password
      |> get_marias_table
      |> get_sequenced_libids
      |> List.enum |> Set.of_enum
    in

    let already_downloaded =
      PGSQL(dbh) "SELECT sl_id FROM th17_sample"
      |> List.enum |> Set.of_enum
    in

    Set.diff available already_downloaded |> Set.enum |> List.of_enum


  let no_fastq_file sequme_root dbh =
    let samples_lanes = PGSQL(dbh)
      "SELECT s.sl_id, l.id FROM th17_sample as s, th17_lane as l
       WHERE l.sample_id = s.id"
    in

    let pred (_,lane_id) =
      let dir = List.reduce Filename.concat
        [sequme_root; "db"; "th17"; "lane"; Int32.to_string lane_id]
      in

      let files =
        dir |> Sys.files_of
        |> Enum.filter (flip Filename.check_suffix ".fastq")
        |> List.of_enum
      in

      match files with
        | [] -> false
        | _ -> true
    in

    samples_lanes |> List.filter (pred |- not)


  let is_paired_end sequme_root id =
    let inp = open_in (fastq_file_path_of_id sequme_root id) in
    let ans =
      inp
      |> Biocaml.Fastq.enum_input
      |> Enum.get |> Option.get
      |> Tuple4.second |> String.length
      |> ((=) 72)
    in
    close_in inp;
    ans

end

module TopHat = struct

  type t = {
    id : int32;
    exec_path : string;
    version : string;
    min_anchor_length : int32 option;
    solexa1_3_quals : bool;
    num_threads : int32 option;
    max_multihits : int32 option;
    no_coverage_search : bool;
    coverage_search : bool;
    butterfly_search : bool;
    gtf_id : int32 option;
    no_novel_juncs : bool;
    index_base : string;
    lane : Lane.t;
    started : timestamptz option;
    finished : timestamptz option;
    status : string;
    note : string
  }

  let all_ids dbh = PGSQL(dbh) "SELECT id FROM th17_tophat"

  let of_id dbh id =
    PGOCaml.begin_work dbh;
    let ans =
      match PGSQL(dbh)
        "SELECT id,exec_path,version,min_anchor_length,
                solexa1_3_quals,num_threads,max_multihits,
                no_coverage_search,coverage_search,
                butterfly_search,gtf_id,no_novel_juncs,
                index_base,lane_id,started,finished,
                status,note
         FROM th17_tophat WHERE id=$id"
      with
        | (id,exec_path,version,min_anchor_length,
                solexa1_3_quals,num_threads,max_multihits,
                no_coverage_search,coverage_search,
                butterfly_search,gtf_id,no_novel_juncs,
                index_base,lane_id,started,finished,
                status,note)::[]
          -> Some {
            id;exec_path;version;min_anchor_length;
            solexa1_3_quals;num_threads;max_multihits;
            no_coverage_search;coverage_search;
            butterfly_search;gtf_id;no_novel_juncs;
            index_base;
            lane = Lane.of_id dbh lane_id |> Option.get;
            started;finished;
            status;note}
        | [] -> None
        | _ -> assert false
    in
    PGOCaml.commit dbh;
    ans

  let run conf dbh sl_id =
    let exec = "/share/apps/tophat/1.2.0/tophat-1.2.0.Linux_x86_64/tophat" in
    let version = "TopHat v1.2.0" in
    let min_anchor_length = 10l in
    let solexa1_3_quals = true in
    let num_threads = 6l in
    let max_multihits = 20l in
    let no_coverage_search = false in
    let coverage_search = false in
    let butterfly_search = false in
    let gtf_name = "refGeneAnnot.gtf" in
    let gtf_id = CachedFile.id_of_name dbh gtf_name in
    let no_novel_juncs = true in
    let index_base = "mm9" in
    let lane_id = Lane.id_of_sl_id dbh sl_id in
    let started = Util.now() in
    let status = "in_progress" in
    let note = "" in

    PGOCaml.begin_work dbh;
    PGSQL(dbh)
      "INSERT INTO th17_tophat
       (exec_path,version,min_anchor_length,solexa1_3_quals,num_threads,
        max_multihits,no_coverage_search,coverage_search,butterfly_search,
        gtf_id,no_novel_juncs,index_base,lane_id,started,status,note)
       VALUES
       ($exec,$version,$min_anchor_length,$solexa1_3_quals,$num_threads,
        $max_multihits,$no_coverage_search,$coverage_search,$butterfly_search,
        $gtf_id,$no_novel_juncs,$index_base,$lane_id,$started,$status,$note)";
    let tophat_id = PGOCaml.serial4 dbh "th17_tophat_id_seq" in
    PGOCaml.commit dbh;

    let sequme_root = Map.StringMap.find "sequme_root" conf in
    let outdir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "tophat"; Int32.to_string tophat_id]
    in
    Unix.mkdir outdir 0o755;
    let tophat_outdir = Filename.concat outdir "tophat_out" in

    let cmd = TopHat.make_cmd ~exec
      ~min_anchor_length:(Int32.to_int min_anchor_length)
      ~solexa1_3_quals
      ~num_threads:(Int32.to_int num_threads)
      ~max_multihits:(Int32.to_int max_multihits)
      ~no_coverage_search
      ~coverage_search
      ~butterfly_search
      ~gtf:(CachedFile.path_of_id sequme_root dbh gtf_id)
      ~no_novel_juncs
      ~output_dir:tophat_outdir
      (Bowtie.path_of_index conf index_base)
      [Lane.fastq_file_path_of_id sequme_root lane_id]
      []
    in

    let job_name = sprintf "tophat_%ld" tophat_id |> flip String.right 15 in
    let resource_list = "nodes=1:ppn=8,mem=14gb" in
    let pbs_outdir = Filename.concat outdir "pbs_out" in
    let cmds = [
      TopHat.cmd_to_string cmd;
      "";
      sprintf "chmod 755 %s" tophat_outdir;
      sprintf "chmod 644 %s/*" tophat_outdir
    ]
    in
    Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds


  let post_process sequme_root dbh id =
    let accepted_hits_file = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "tophat"; Int32.to_string id; "tophat_out"; "accepted_hits.bam"]
    in

    let status =
      if Sys.file_exists accepted_hits_file
      then "sucess" else "failed"
    in PGSQL(dbh) "UPDATE th17_tophat SET status=$status WHERE id=$id";

    let finished = Util.file_last_modified_time accepted_hits_file in
    PGSQL(dbh) "UPDATE th17_tophat SET finished=$finished WHERE id=$id"


  let delete conf dbh id =
    match PGSQL(dbh) "SELECT id FROM th17_tophat WHERE id=$id" with
      | [] -> Error (sprintf "tophat run with ID %ld unknown" id) |> raise
      | _::_::_ -> assert false
      | _::[] ->
          PGSQL(dbh) "DELETE FROM th17_tophat WHERE id=$id";
          let dir = List.reduce Filename.concat
            [Map.StringMap.find "sequme_root" conf; "db"; "th17"; "tophat"; Int32.to_string id]
          in
          sprintf "rm -rf %s" dir |> Sys.command |> ignore

end

module Bowtie = struct

  type t = {
    id : int32;
    exec_path : string;
    version : string;
    index_base : string;
    k : int32 option;
    best : bool;
    sam : bool;
    num_threads : int32 option;
    lane : Lane.t;
    started : timestamptz option;
    finished : timestamptz option;
    status : string;
    note : string
  }

  let all_ids dbh = PGSQL(dbh) "SELECT id FROM th17_bowtie"

  let of_id dbh id =
    PGOCaml.begin_work dbh;
    let ans =
      match PGSQL(dbh)
        "SELECT id,exec_path,version,index_base,k,best,sam,
                num_threads,lane_id,started,finished,
                status,note
         FROM th17_bowtie WHERE id=$id"
      with
        | (id,exec_path,version,index_base,k,best,sam,
          num_threads,lane_id,started,finished,
          status,note)::[]
          -> Some {
            id;exec_path;version;index_base;
            k;best;sam;num_threads;
            lane = Lane.of_id dbh lane_id |> Option.get;
            started;finished;
            status;note
          }
        | [] -> None
        | _ -> assert false
    in
    PGOCaml.commit dbh;
    ans


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
    let note = "" in

    let sequme_root = Map.StringMap.find "sequme_root" conf in
    lane_id |> Lane.fastq_file_path_of_id sequme_root |> ignore;

    PGOCaml.begin_work dbh;
    PGSQL(dbh)
      "INSERT INTO th17_bowtie
       (exec_path,version,index_base,k,best,sam,num_threads,lane_id,started,status,note)
       VALUES
       ($exec,$version,$index_base,$k,$best,$sam,$num_threads,$lane_id,$started,$status,$note)"
    ;
    let bowtie_id = PGOCaml.serial4 dbh "th17_bowtie_id_seq" in
    PGOCaml.commit dbh;

    let outdir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "bowtie"; Int32.to_string bowtie_id]
    in
    Unix.mkdir outdir 0o755;

    let hit_file = Filename.concat outdir (sl_id ^ ".sam") in

    let cmd = Bowtie.make_cmd ~exec
      ~ebwt:(Bowtie.path_of_index conf index_base)
      ~k:(Int32.to_int k)
      ~best ~sam
      ~threads:(Int32.to_int num_threads)
      ~hit:hit_file
      ~reads:(Lane.fastq_file_path_of_id sequme_root lane_id)
    in

    let job_name = sprintf "bowtie_%ld" bowtie_id |> flip String.right 15 in
    let resource_list = "nodes=1:ppn=8,mem=14gb" in
    let pbs_outdir = Filename.concat outdir "pbs_out" in
    let cmds = [
      Bowtie.cmd_to_string cmd;
    ]
    in
    Pbs.make_and_run ~resource_list ~job_name pbs_outdir cmds


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
            [Map.StringMap.find "sequme_root" conf; "db"; "th17"; "bowtie"; Int32.to_string id]
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

  let stderr_file sequme_root id =
    List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "bowtie";
       Int32.to_string id; "pbs_out"; "stderr.txt"
      ]


  let was_success sequme_root id =
    stderr_file sequme_root id
    |> File.lines_of
    |> Enum.exists (fun x ->
         String.exists x "Reported"
         && String.exists x "alignments to 1 output stream(s)"
       )
    

  let post_process sequme_root dbh id =
    let status =
      if was_success sequme_root id then "success" else "failed"
    in PGSQL(dbh) "UPDATE th17_bowtie SET status=$status WHERE id=$id";

    let finished = Util.file_last_modified_time (stderr_file sequme_root id) in
    PGSQL(dbh) "UPDATE th17_bowtie SET finished=$finished WHERE id=$id"

end

module Macs = struct

  type t = {
    id : int32;
    exec_path : string;
    version : string;
    started : timestamptz option;
    finished : timestamptz option;
    status : string;
    note : string;
    name : string;
    format : string;
    pvalue : string;
    mfold : (int32 * int32) option;
    tsize : int32 option;
    gsize : string;
    bw : int32 option;
    wig : bool;
    space : int32 option;
    control : Bowtie.t;
    treatment : Bowtie.t
  }

  let all_ids dbh = PGSQL(dbh) "SELECT id FROM th17_macs"

  let of_id dbh id =
    PGOCaml.begin_work dbh;
    let ans =
      match PGSQL(dbh)
        "SELECT id,exec_path,version,started,finished,status,note,
                name,format,
                pvalue,mfold_low,mfold_high,tsize,gsize,bw,
                wig,space,control_id,treatment_id
         FROM th17_macs WHERE id=$id"
      with
        | (id,exec_path,version,started,finished,status,note,
          name,format,
          pvalue,mfold_low,mfold_high,tsize,gsize,bw,
          wig,space,control_id,treatment_id)::[] ->
            Some {
              id;exec_path;version;started;finished;status;note;
              name;format;pvalue;
              mfold = (
                match mfold_low,mfold_high with
                  | Some l, Some h -> Some (l,h)
                  | None, None -> None
                  | _ -> assert false
              );
              tsize;gsize;bw;wig;space;
              control = Bowtie.of_id dbh control_id |> Option.get;
              treatment = Bowtie.of_id dbh treatment_id |> Option.get
            }
        | [] -> None
        | _ -> assert false
    in
    PGOCaml.commit dbh;
    ans


  let run conf dbh treatment control =
    let sequme_root = Map.StringMap.find "sequme_root" conf in

    let exec = "/home/aa144/local/python/bin/macs14" in
    let version = "macs14 1.4.0rc2 20110214 (Valentine) patched" in
    let name = sprintf "%s_%s" treatment control in
    let format = "sam" in
    let pvalue = "1e-10" in
    let mfold_low = 15l in
    let mfold_high = 30l in
    let tsize = 36l in
    let gsize = "mm" in
    let bw = 200l in
    let wig = true in
    let space = 1l in

    let treatment_bowtie_id = Bowtie.any_id_of_sl_id dbh treatment in
    let treatment_sam_file = Bowtie.sam_file_path_of_id sequme_root treatment_bowtie_id in
    let control_bowtie_id = Bowtie.any_id_of_sl_id dbh control in
    let control_sam_file = Bowtie.sam_file_path_of_id sequme_root control_bowtie_id in

    let note = "" in
    let status = "in_progress" in
    let started = Util.now() in

    PGSQL(dbh)
      "INSERT INTO th17_macs
       (exec_path,version,started,status,note,name,format,pvalue,
        mfold_high,mfold_low,tsize,gsize,bw,wig,space,
        control_id,treatment_id)
       VALUES
       ($exec,$version,$started,$status,$note,$name,$format,$pvalue,
        $mfold_high,$mfold_low,$tsize,$gsize,$bw,$wig,$space,
        $control_bowtie_id,$treatment_bowtie_id)"
    ;
    let macs_id = PGOCaml.serial4 dbh "th17_macs_id_seq" in

    let outdir = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "macs"; Int32.to_string macs_id] in

    Unix.mkdir outdir 0o755;

    let macs_cmd = Macs.make_cmd ~exec
      ~name ~format ~pvalue ~mfold:(mfold_low,mfold_high)
      ~tsize ~gsize ~bw
      ~wig ~space
      ~control:control_sam_file ~treatment:treatment_sam_file () in

    let macs_outdir = Filename.concat outdir "macs_out" in
    Unix.mkdir macs_outdir 0o755;

    let cmds = [
      sprintf "cd %s" macs_outdir;
      "";
      Macs.cmd_to_string macs_cmd;
    ] in

    let job_name = sprintf "macs_%ld" macs_id |> flip String.right 15 in
    let pbs_outdir = Filename.concat outdir "pbs_out" in
    Pbs.make_and_run ~job_name pbs_outdir cmds


  let delete conf dbh id =
    match PGSQL(dbh) "SELECT id FROM th17_macs WHERE id=$id" with
      | [] -> Error (sprintf "MACS run %ld unknown" id) |> raise
      | _::_::_ -> assert false
      | _::[] ->
          PGSQL(dbh) "DELETE FROM th17_macs WHERE id=$id";
          let dir = List.reduce Filename.concat
            [Map.StringMap.find "sequme_root" conf; "db"; "th17"; "macs"; Int32.to_string id]
          in
          sprintf "rm -rf %s" dir |> Sys.command |> ignore


  let last_info_datetime sequme_root id =
    let stderr_file = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "macs";
       Int32.to_string id; "pbs_out"; "stderr.txt"]
    in

    let get_time prev_time line =
      match String.starts_with line "INFO" with
        | false -> prev_time
        | true ->
            let open CalendarLib in
            let datetime =
              line |> flip String.left 33
              |> Printer.Calendar.from_fstring "INFO  @ %a, %d %b %Y %H:%M:%S"
            in
            Some (datetime, Time_Zone.Local)
    in

    stderr_file |> File.lines_of
    |> Enum.fold get_time None


  let was_success sequme_root id =
    let stderr_file = List.reduce Filename.concat
      [sequme_root; "db"; "th17"; "macs";
       Int32.to_string id; "pbs_out"; "stderr.txt"]
    in
    stderr_file |> File.lines_of
    |> Enum.exists (flip String.exists "Done! Check the output files!")

  let post_process sequme_root dbh id =
    let status =
      if was_success sequme_root id then "success" else "failed"
    in PGSQL(dbh) "UPDATE th17_macs SET status=$status WHERE id=$id";

    match last_info_datetime sequme_root id with
      | None -> ()
      | Some x -> PGSQL(dbh) "UPDATE th17_macs SET finished=$x WHERE id=$id"

end
