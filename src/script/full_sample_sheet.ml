#!/usr/bin/env ocamlscript
Ocaml.ocamlflags := [ "-thread"];;
Ocaml.packs := [ "sequme";  ];;
--

open Sequme_std

let illumina_barcodes = [
  1, "ATCACG";
  2, "CGATGT";
  3, "TTAGGC";
  4, "TGACCA";
  5, "ACAGTG";
  6, "GCCAAT";
  7, "CAGATC";
  8, "ACTTGA";
  9, "GATCAG";
  10, "TAGCTT";
  11, "GGCTAC";
  12, "CTTGTA"
]
let bioo_barcodes = [
  1 ,"CGATGT";
  2 ,"TGACCA";
  3 ,"ACAGTG";
  4 ,"GCCAAT";
  5 ,"CAGATC";
  6 ,"CTTGTA";
  7 ,"ATCACG";
  8 ,"TTAGGC";
  9 ,"ACTTGA";
  10,"GATCAG";
  11,"TAGCTT";
  12,"GGCTAC";
  13,"AGTCAA";
  14,"AGTTCC";
  15,"ATGTCA";
  16,"CCGTCC";
  17,"GTAGAG";
  18,"GTCCGC";
  19,"GTGAAA";
  20,"GTGGCC";
  21,"GTTTCG";
  22,"CGTACG";
  23,"GAGTGG";
  24,"GGTAGC";
  25,"ACTGAT";
  26,"ATGAGC";
  27,"ATTCCT";
  28,"CAAAAG";
  29,"CAACTA";
  30,"CACCGG";
  31,"CACGAT";
  32,"CACTCA";
  33,"CAGGCG";
  34,"CATGGC";
  35,"CATTTT";
  36,"CCAACA";
  37,"CGGAAT";
  38,"CTAGCT";
  39,"CTATAC";
  40,"CTCAGA";
  41,"GCGCTA";
  42,"TAATCG";
  43,"TACAGC";
  44,"TATAAT";
  45,"TCATTC";
  46,"TCCCGA";
  47,"TCGAAG";
  48,"TCGGCA";
]
let () =
  let flowcell = Sys.argv.(1) in
  let config =
    List.map (List.tl_exn (List.tl_exn (Array.to_list Sys.argv)))
      ~f:(function
        | "I2" -> 2, illumina_barcodes 
        | "I3" -> 3, illumina_barcodes 
        | "I4" -> 4, illumina_barcodes 
        | "I5" -> 5, illumina_barcodes 
        | "I6" -> 6, illumina_barcodes 
        | "I7" -> 7, illumina_barcodes 
        | "I8" -> 8, illumina_barcodes 
        | "B2" -> 2, bioo_barcodes 
        | "B3" -> 3, bioo_barcodes 
        | "B4" -> 4, bioo_barcodes 
        | "B5" -> 5, bioo_barcodes 
        | "B6" -> 6, bioo_barcodes 
        | "B7" -> 7, bioo_barcodes 
        | "B8" -> 8, bioo_barcodes 
        | s ->
          failwith (sprintf  "Can't understand %S" s)
      ) in
  let head = 
    sprintf
      "FCID,Lane,SampleID,SampleRef,Index,Description,Control,Recipe,Operator,SampleProject
%s,1,PhiX,,,,Y,,,PhiX
" flowcell in
  printf "%s" head;
  List.iter config ~f:(fun (lane, barcodes) ->
    List.iter barcodes ~f:(fun (id, barcode) ->
      printf "%s,%d,I%02d%s,,%s,,N,,,I%02d%sLane%d\n"
        flowcell lane id barcode barcode id barcode lane;
    );
  );

  ()
