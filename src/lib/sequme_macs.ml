open Sequme_internal_pervasives

exception Error of string

type cmd = {
  exec : string;
  name : string option;
  format : string option;
  pvalue : string option;
  mfold : (int32 * int32) option;
  tsize : int32 option;
  gsize : string option;
  bw : int32 option;
  wig : bool;
  space : int32 option;
  control : string;
  treatment : string;
}

let make_cmd
    ?(exec="macs")
    ?name ?format ?pvalue ?mfold ?tsize ?gsize ?bw
    ?(wig=false) ?space
    ~control ~treatment ()
    =
  {exec; name; format; pvalue; mfold; tsize; gsize; bw; wig; space; control; treatment}

let cmd_to_string cmd =
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %ld" opt x in
  let i' opt x = match x with None -> "" | Some x -> sprintf " --%s=%ld" opt x in
  let ii opt x = match x with None -> "" | Some (x1,x2) -> sprintf " -%c %ld,%ld" opt x1 x2 in
  let s opt x = match x with None -> "" | Some x -> sprintf " -%c %s" opt x in
  String.concat ~sep:"" [
    cmd.exec;
    s 'n' cmd.name;
    s 'f' cmd.format;
    s 'p' cmd.pvalue;
    ii 'm' cmd.mfold;
    i 's' cmd.tsize;
    s 'g' cmd.gsize;
    i' "bw" cmd.bw;
    (if cmd.wig then " -w" else "");
    i' "space" cmd.space;
    sprintf " -t %s" cmd.treatment;
    sprintf " -c %s" cmd.control;
  ]
