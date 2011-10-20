(** Illumina specific features. *)

(** Features specific to Illumina FASTQ files. Meant to be used in
    conjunction with the {!Biocaml.Fastq} module. *)
module Fastq : sig
  exception Error of string

  type sequence_id = {
    instrument : string;
    run_number : int;
    flowcell_id : string;
    lane : int;
    tile : int;
    x_pos : int;
    y_pos : int;
    read : int;
    is_filtered : bool;
    control_number : int;
    index : string
  }

  val sequence_id_of_string : string -> sequence_id
    (** Parse sequence ID in string format, as generated by
        {!Biocaml.Fastq}, according to the format followed by Illumina. *)

end

(** Barcodes. *)
module Barcode : sig
  exception Error of string
  type t
  val of_int : int -> t
  val of_ad_code : string -> t
  val of_seq : string -> t
  val to_ad_code : t -> string
  val to_seq : t -> string
end


(** Support for [SampleSheet]s. *)
module SampleSheet : sig
  exception Error of string

  type record = {
    flowcell_id : string;
    lane : int;
    sample_id : string;
    sample_ref : string;
    barcode : Barcode.t;
    description : string;
    control : bool;
    recipe : string;
    operator : string;
    project : string
  }

  type t = record list

  val of_file : string -> t
  val group_by_sample_id : t -> record list BatMap.StringMap.t
  val find_lane_barcode : t -> int -> Barcode.t -> record option
end


(** Support for Demultiplex_Stats.htm file. *)
module DemultiplexStats : sig

  exception Error of string

  (** Information regarding each barcode. *)
  module Barcode : sig
    type t = {
      lane : int;
      sample_id : string;
      sample_ref : string;
      index : string;
      description : string;
      control : bool;
      project : string;
      yield : int;
      percent_pass_filter : float;
      num_reads : int;
      percent_of_raw_clusters_per_lane : float;
      percent_perfect_index_reads : float;
      percent_one_mismatch_reads : float;
      percent_gte_Q30 : float;
      mean_quality_score : float;
    }
  end

  (** Information regarding each sample. *)
  module Sample : sig
    type t = {
      sample_id : string;
      recipe : string;
      operator : string;
      directory : string
    }
  end

  type flowcell = string
  type software = string
 
  type t = flowcell * Barcode.t list * Sample.t list * software

  val of_rendered_html : string -> t
    (** Parses rendered Demultiplex_Stats.htm file. Open the file in a
        browser, select all, copy selection, paste into Excel, and
        then export as tab delimited file. Obviously this function
        should be replaced with one that directly parses the html
        file. And thus, this function also does not do rigorous error
        checking or provide great error messages. It was implemented
        for quick and dirty use. *)
   
end
