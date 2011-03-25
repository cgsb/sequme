open Batteries_uni;; open Printf

exception Error of string

type cmd = {
  exec : string;
  format : string option;
  pvalue : string option;
  mfold : (int32 * int32) option;
  tsize : int32 option;
  gsize : string option;
  bw : int32 option;
  control : string;
  treatment : string;
}

let make_cmd
    ?(exec="macs")
    ?format ?pvalue ?mfold ?tsize ?gsize ?bw
    ~control ~treatment
    =
  {exec; format; pvalue; mfold; tsize; gsize; bw; control; treatment}

let cmd_to_string cmd =
  let i opt x = match x with None -> "" | Some x -> sprintf " -%c %ld" opt x in
  let i' opt x = match x with None -> "" | Some x -> sprintf " -%s %ld" opt x in
  let ii opt x = match x with None -> "" | Some (x1,x2) -> sprintf " -%c %ld,%ld" opt x1 x2 in
  let s opt x = match x with None -> "" | Some x -> sprintf " -%c %s" opt x in
  String.concat "" [
    cmd.exec;
    s 'f' cmd.format;
    s 'p' cmd.pvalue;
    ii 'm' cmd.mfold;
    i 's' cmd.tsize;
    s 'g' cmd.gsize;
    i' "bw" cmd.bw;
    sprintf " -t %s" cmd.treatment;
    sprintf " -c %s" cmd.control;
  ]
