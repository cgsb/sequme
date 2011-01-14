open Batteries_uni;; open Biocaml;; open Printf

let usage = sprintf "\
Usage:
  %s <paired_end.fastq> <left_out.fastq> <right_out.fastq>\
" Sys.argv.(0)

exception Error of string

(** Print fastq record to given output. *)
let fprint cout (a,b,c,d) =
  fprintf cout "%s\n%s\n%s\n%s\n" a b c d

(** Return ID uniquely identifying the read in given fastq record. *)
let get_id (title,_,_,_) = String.nsplit title ":" |> List.take 6

(** Return true if given fastq record contains a good sequence. *)
let good_seq (_,seq,_,_) = String.contains seq '.' |> not


(** Return [true] if given fastq record is a left mate, [false]
    if it is a right mate. Raise [Error] if there is an error. *)
let is_left (title,_,_,_) : bool =
  match String.nsplit title ":" with
    | [_;_;_;_;_;_;lh;_] ->
        if lh = "1" then true
        else if lh = "2" then false
        else Error "expected '1' or '2' in 7th field of title" |> raise
    | _ -> Error "expected 8 fields in title" |> raise


(** Split paired-end fastq data into left and right mates. *)
let split_fastq (i:IO.input) (l : unit IO.output) (r : unit IO.output) : unit =
  i |> Fastq.enum_input
  |> Enum.iter (fun x -> fprint (if is_left x then l else r) x)


(** Filter paired-end fastq data, given separately in two inputs,
    keeping only those in which both mates are good. A good mate is
    one that does not contain any dots in its sequence. Filtered
    output is written to given respective outputs.

    Raise [Error] if left and right inputs are not ordered identically
    be read ID, or if inputs contain a different number of records. *)
let filterlr (inl:IO.input) (inr:IO.input) (outl : unit IO.output) (outr : unit IO.output) : unit =
  let inl,inr = Pair.map Fastq.enum_input (inl,inr) in
  let f xl xr =
    if get_id xl <> get_id xr then
      Error "read IDs differ" |> raise
    else if good_seq xl && good_seq xr then
      (fprint outl xl; fprint outr xr)
  in
  Enum.iter2 f inl inr;
  if not (Enum.is_empty inl && Enum.is_empty inr) then
    Error "inputs contain different number of reads" |> raise
  

let main in_file lfile rfile =
  (* split in_file into temporary left and right mate files *)
  let tmpl,tmpr =
    let open Filename in
    let make_tmp file = temp_file ~temp_dir:(dirname file) "" ("_unfiltered_" ^ basename file) in
    let tmpl,tmpr = Pair.map make_tmp (lfile,rfile) in
    let (in_file',tmpl',tmpr') as ich' = open_in in_file, open_out tmpl, open_out tmpr in
    let finalize () = close_in in_file'; close_out tmpl'; close_out tmpr' in
    finally finalize (fun (x,y,z) -> split_fastq x y z) ich';
    tmpl,tmpr
  in

  (* filter temporary files created above *)
  let inl,inr = Pair.map open_in (tmpl,tmpr) in
  let mode = [`create; `excl; `text] in
  let perm = File.unix_perm 0o644 in
  let outl,outr = Pair.map (open_out ~mode ~perm) (lfile,rfile) in
  let finalize () = close_in inl; close_in inr; close_out outl; close_out outr in
  finally finalize (fun (a,b,c,d) -> filterlr a b c d) (inl,inr,outl,outr);
  Unix.unlink tmpl; Unix.unlink tmpr

;;
if Array.length Sys.argv = 4 then 
  try main Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
  with e -> Printexc.print stderr e
else
  printf "%s\n" usage
