(** Support for Mitzi's ledger files. *)
open Printf;; open Batteries_uni

module Map = Map.StringMap

(** A ledger is a mapping from attibutes to their values. Note
    sections are not retained. *)
type ledger = string Map.t

type dbh = (string, bool) Batteries_uni.Hashtbl.t PGOCaml.t

(** {6 Query Ledger Files} *)

(** Absolute paths of all ledger files in Mitzi's directory on
    avalon. *)
let ledger_files : string list =
  let dir = "/data/cgsb/solid_runs/ledger" in
  dir |> Sys.files_of
              |> Enum.filter (flip Filename.check_suffix ".info")
              |> Enum.filter (not -| flip List.mem ["template.info"; "test_example.info"])
              |> Enum.map (Filename.concat dir)
              |> List.of_enum

(** [of_file ledger_file] returns all attribute values in
    [ledger_file]. *)
let of_file file : ledger =
  let add_line ans line =
    match String.nsplit line "=" with
      | [x;y] ->
          let x = String.strip x in
          let y = String.strip y in
          if Map.mem x ans then
            failwith (sprintf "%s: %s tag occurs more than once" file x)
          else
            Map.add x y ans
      | _ -> ans
  in
  
  file |> File.lines_of
              |> Enum.fold add_line Map.empty             

(** [get_sections ledger_file] returns list of sections found in
    [ledger_file]. *)
let get_sections ledger : string list =
  let f ans line =
    let line = String.strip line in
    if String.starts_with line "[" && String.ends_with line "]" then
      if List.mem line ans then
        failwith (sprintf "%s: %s section occurs multiple times" ledger line)
      else
        line::ans
    else
      ans
  in
  ledger |> File.lines_of |> Enum.fold f [] |> List.rev

(** [parse_section s ledger_file] will return tag-value pairs in
    section [s] from [ledger_file], where [s] should be something like
    "[qa]", "[raw]", etc. *)
let parse_section section file : string Map.t =
  let f (ans,started,ended) line =
    let line = String.strip line in
    if String.starts_with line "#" then
      ans,started,ended
    else
      match started,ended with
        | false,false ->
            if line = section then
              ans,true,false
            else
              ans,false,false
        | true,false ->
            if line = "" then
              ans,true,true
            else (
              match String.nsplit line "=" with
                | [x;y] ->
                    let x = String.strip x in
                    let y = String.strip y in
                    if Map.mem x ans then
                      failwith (sprintf "%s: %s section has field %s more than once" file section x)
                    else
                      Map.add x y ans, true, false
                | _ -> failwith (sprintf "%s: %s section did not end with empty line" file section)
            )
        | true,true -> ans,true,true
        | false,true -> assert false
  in
  let ans,started,ended = file |> File.lines_of |> Enum.fold f (Map.empty,false,false) in
  if not started then
    (printf "WARNING: %s: %s section not found\n" file section; Map.empty)
  else
    ans

let get_sample_id dbh (ledger:ledger) : int32 =
  let name = Map.find "name" ledger in
  match PGSQL(dbh) "SELECT id FROM cgsb_sample WHERE name=$name" with
    | x::[] -> x
    | _ -> assert false


(** {6 Port Ledgers to Database} *)

let lab_to_user_id lab = match lab with
  | "Birnbaum lab" -> 20l
  | "Borowsky lab" -> 22l
  | "Fitch lab" -> 23l
  | "Gresham lab" -> 25l
  | "Piano lab" -> 21l
  | "Purugganan lab" -> 26l
  | "rockman_lab"
  | "Rockman lab" -> 24l
  | _ -> assert false


(** Insert relevant values from \[run\] sections of [ledger_files] into
    the database. *)
let port_samples (dbh:dbh) =
  let add_sample dbh file =
    let ledger = parse_section "[run]" file in
    let created = Util.now() in
    let last_modified = created in
    let made_by_id = Map.find "lab" ledger |> lab_to_user_id in
    let name = Map.find "name" ledger in
    let sample_name = Map.find "sample_name" ledger in
    let read_type = Map.find "readtype" ledger in
    let note = "" in
    try PGSQL(dbh)
      "INSERT INTO cgsb_sample
       (created,last_modified,made_by_id,name,sample_name,read_type,note)
       VALUES
       ($created,$last_modified,$made_by_id,$name,$sample_name,$read_type,$note)"
    with e ->
      printf "%s: caused following error\n%s\n" file (Printexc.to_string e)
  in
  ledger_files |> List.iter (add_sample dbh)

let port_section_qa (dbh:dbh) =
  let one_ledger dbh file =
    let ledger = of_file file in
    let find x = try Map.find x ledger with Not_found -> "Not_found" in
    let qa_trims = find "qa_trims" in
    let fifty_filter_csfasta_name = find "50.filter_csfasta_name" in
    let a50_filter_csfasta_name = find "a50.filter_csfasta_name" in
    let qa_jpeg = find "qa_jpeg" in
    let notrim_filter_csfasta_name = find "notrim.filter_csfasta_name" in
    let qa_trim_tab = find "qa_trim_tab" in
    let trim35_filter_csfasta_name = find "trim35.filter_csfasta_name" in
    let ct_called = find "ct_called" in
    let filter_csfasta_name = find "filter_csfasta_name" in
    let filter_qual_name = find "filter_qual_name" in
    let qa_jpg = find "qa_jpg" in
    let trim25_filter_csfasta_name = find "trim25.filter_csfasta_name" in
    let trim30_filter_csfasta_name = find "trim30.filter_csfasta_name" in

    let sample_id = get_sample_id dbh ledger in
    PGSQL(dbh)
      "INSERT INTO cgsb_section_qa
       (sample_id,qa_trims,fifty_filter_csfasta_name,a50_filter_csfasta_name,
        qa_jpeg,notrim_filter_csfasta_name,qa_trim_tab,trim35_filter_csfasta_name,
        ct_called,filter_csfasta_name,filter_qual_name,qa_jpg,
        trim25_filter_csfasta_name,trim30_filter_csfasta_name)
       VALUES
       ($sample_id,$qa_trims,$fifty_filter_csfasta_name,$a50_filter_csfasta_name,
        $qa_jpeg,$notrim_filter_csfasta_name,$qa_trim_tab,$trim35_filter_csfasta_name,
        $ct_called,$filter_csfasta_name,$filter_qual_name,$qa_jpg,
        $trim25_filter_csfasta_name,$trim30_filter_csfasta_name)"
  in
  ledger_files |> List.iter (one_ledger dbh)

let port_section_raw (dbh:dbh) =
  let one_ledger dbh file =
    let ledger = of_file file in
    let find x = try Map.find x ledger with Not_found -> "Not_found" in
    let csfasta_name = find "csfasta_name" in
    let ct_raw = find "ct_raw" in
    let qual_name = find "qual_name" in
    let rawtgz = find "rawtgz" in
    let readsdir = find "readsdir" in

    let sample_id = get_sample_id dbh ledger in
    PGSQL(dbh)
      "INSERT INTO cgsb_section_raw
       (sample_id,csfasta_name,ct_raw,qual_name,rawtgz,readsdir)
       VALUES
       ($sample_id,$csfasta_name,$ct_raw,$qual_name,$rawtgz,$readsdir)"
  in
  ledger_files |> List.iter (one_ledger dbh)

let port_section_align (dbh:dbh) =
  let one_ledger dbh file =
    let ledger = of_file file in
    let find x = try Map.find x ledger with Not_found -> "Not_found" in
    let reads_aligned_multi = find "reads_aligned_multi" in
    let save_unaligned = find "save_unaligned" in
    let min325_reads_aligned = find "min325.reads_aligned" in
    let min325_reads_aligned_uniquely = find "min325.reads_aligned_uniquely" in
    let sam_1best = find "sam_1best" in
    let sam_multimap = find "sam_multimap" in
    let pct_aligned = find "pct_aligned" in
    let total_alignments = find "total_alignments" in
    let aligndir = find "aligndir" in
    let aligner = find "aligner" in
    let aligner_mem = find "aligner_mem" in
    let aligner_path = find "aligner_path" in
    let aligner_settings = find "aligner_settings" in
    let ds9bin = find "ds9bin" in
    let jmem = find "jmem" in
    let min_align_score = find "min_align_score" in
    let reads_aligned = find "reads_aligned" in
    let reads_aligned_uniquely = find "reads_aligned_uniquely" in
    let refseq = find "refseq" in
    let refseq_fasta = find "refseq_fasta" in
    let refseq_index = find "refseq_index" in
    let trim_len = find "trim_len" in

    let sample_id = get_sample_id dbh ledger in
    PGSQL(dbh)
      "INSERT INTO cgsb_section_align
       (sample_id,reads_aligned_multi,save_unaligned,min325_reads_aligned,min325_reads_aligned_uniquely,sam_1best,sam_multimap,pct_aligned,total_alignments,aligndir,aligner,aligner_mem,aligner_path,aligner_settings,ds9bin,jmem,min_align_score,reads_aligned,reads_aligned_uniquely,refseq,refseq_fasta,refseq_index,trim_len)
       VALUES
       ($sample_id,$reads_aligned_multi,$save_unaligned,$min325_reads_aligned,$min325_reads_aligned_uniquely,$sam_1best,$sam_multimap,$pct_aligned,$total_alignments,$aligndir,$aligner,$aligner_mem,$aligner_path,$aligner_settings,$ds9bin,$jmem,$min_align_score,$reads_aligned,$reads_aligned_uniquely,$refseq,$refseq_fasta,$refseq_index,$trim_len)"
  in
  ledger_files |> List.iter (one_ledger dbh)

