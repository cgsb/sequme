(** Th17 Metadata. As stored on Google Doc. *)
open Batteries_uni;; open Biocaml;; open Printf

type record = {
  sl_id : string;
  application : string;
  organism : string;
  control_sl_id : string;
  read_type : string;
  phred_offset : string;
  download : string;
  bowtie : string;
  htseq_count : string;
  tophat : string;
  cufflinks : string;
  fpkms_ml : string;
  sam_keep_name_only_ml : string;
  macs : string;
}

type t = record list

let columns = [
  "Library_ID";
  "Sample_ID";
  "Application";
  "Organism";
  "Control_Sample_ID";
  "Control_Library_ID";
  "Factor";
  "Condition";
  "Genotype";
  "Experiment_Group";
  "Replicate_Group";
  "Time_Point";
  "Read_Type";
  "Read_Length";
  "PHRED offset";
  "Note";
  "Date_Sequenced";
  "Library_Status";
  "Download";
  "Bowtie";
  "htseq-count";
  "Tophat";
  "Cufflinks";
  "fpkms.ml";
  "sam_keep_name_only.ml";
  "MACS";
]

let of_file file =
  let sll = Csv.load ~separator:'\t' file |> Csv.square in
  let cols,data = List.hd sll, List.tl sll in
  assert (columns = cols);
  let itags = "table,header" in
  let _,_,get,e = Table.of_string_list ~itags ~columns data in
    e
    |> Enum.map (fun x ->
         let get = get x in
         {
           sl_id = get "Library_ID";
           application = (
             let x = get "Application" in
             match x with
               | "RNA-Seq" | "ChIP-Seq" | "INPUT_RXLCh" -> x
               | _ -> failwith (sprintf "invalid application %s" x)
           );
           organism = (
             let x = get "Organism" in
             match x with
               | "Mouse" | "Human" -> x
               | _ -> failwith (sprintf "invalid organism %s" x)
           );
           control_sl_id = get "Control_Library_ID";
           read_type = (
             let x = get "Read_Type" in
             match x with
               | "PE" | "SE" -> x
               | _ -> failwith (sprintf "invalid read type %s" x)
           );
           phred_offset = (
             let x = get "PHRED offset" in
             match x with
               | "Q33" | "Q64" -> x
               | _ -> failwith (sprintf "invalid phred offset %s" x)
           );
           download = get "Download";
           bowtie = get "Bowtie";
           htseq_count = get "htseq-count";
           tophat = get "Tophat";
           cufflinks = get "Cufflinks";
           fpkms_ml = get "fpkms.ml";
           sam_keep_name_only_ml = get "sam_keep_name_only.ml";
           macs = get "MACS";
         }
       )
    |> List.of_enum
