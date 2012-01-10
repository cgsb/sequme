(** PBS support. *)
open Sequme_std

exception Error of string

type mail_option = JobAborted | JobBegun | JobEnded

let mail_option_to_char = function
  | JobAborted -> 'a'
  | JobBegun -> 'b'
  | JobEnded -> 'e'

type script = {
  shell: string;
  mail_options : mail_option list;
  user_list : string list;
  resource_list : string option;
  job_name : string option;
  priority : int option;
  stdout_path : string option;
  stderr_path : string option;
  export_qsub_env : bool;
  rerunable : bool option;
  queue: string option;
  commands : string list;
}

let make_script ?(shell="/bin/bash")
    ?(mail_options=[]) ?(user_list=[]) ?resource_list ?job_name
    ?priority
    ?stdout_path ?stderr_path ?(export_qsub_env=false)
    ?rerunable ?queue
    commands
    =
  {
    shell;
    mail_options = List.unique mail_options;
    user_list;
    resource_list;
    job_name;
    priority;
    stdout_path;
    stderr_path;
    export_qsub_env;
    rerunable;
    queue;
    commands
  }

let script_to_string x =
  let e opt = sprintf "#PBS %s\n" opt in
  let s opt x = sprintf "#PBS %s %s\n" opt x in
  let i opt x = sprintf "#PBS %s %d\n" opt x in
  let header = String.concat "" [
    sprintf "#!%s\n" x.shell;
    (match x.mail_options with
      | [] -> ""
      | x ->
          let x = List.map (mail_option_to_char |- string_of_char) x in
          s "-m" (String.concat "" x)
    );
    (match x.user_list with
      | [] -> ""
      | x -> s "-M" (String.concat "," x)
    );
    if x.export_qsub_env then e "-V" else "";
    (match x.rerunable with
      | None -> ""
      | Some x -> s "-r" (if x then "y" else "n")
    );
    (match x.resource_list with None -> "" | Some x -> s "-l" x);
    (match x.priority with None -> "" | Some x -> i "-p" x);
    (match x.stdout_path with None -> "" | Some x -> s "-o" x);
    (match x.stderr_path with None -> "" | Some x -> s "-e" x);
    (match x.job_name with None -> "" | Some x -> s "-N" x);
    (match x.queue with None -> "" | Some q -> s "-q" q);
  ]
  in
  header ^ "\n" ^ (String.concat "\n" x.commands)

let script_to_file script ?mode ?perm file : unit =
  let cout = match mode,perm with
    | None, None -> open_out file
    | Some mode, None -> open_out ~mode file
    | None, Some perm -> open_out ~perm file
    | Some mode, Some perm -> open_out ~mode ~perm file
  in
  finally (fun () -> close_out cout) (fun cout -> fprintf cout "%s\n" (script_to_string script)) cout


let make_and_run ?(resource_list="nodes=1:ppn=8,mem=14gb") ~job_name outdir commands =
  let pbs_stdout_file = Filename.concat outdir "stdout.txt" in
  let pbs_stderr_file = Filename.concat outdir "stderr.txt" in
  let pbs_script_file = Filename.concat outdir "script.pbs" in
  let qsub_out_file = Filename.concat outdir "qsub_out.txt" in

  let script = make_script
    (* ~mail_options:[JobAborted; JobBegun; JobEnded] *)
    (* ~user_list:["ashish.agarwal@nyu.edu"] *)
    ~resource_list
    (* ~priority:(-1024) *)
    ~job_name
    ~stdout_path:pbs_stdout_file
    ~stderr_path:pbs_stderr_file
    ~export_qsub_env:true
    ~rerunable:false
    commands
  in

  Unix.mkdir outdir 0o755;
  script_to_file script ~perm:(File.unix_perm 0o644) pbs_script_file;
  let cmd = sprintf "qsub %s > %s 2>&1" pbs_script_file qsub_out_file in
  print_endline cmd;
  match Sys.command cmd with
    | 0 -> ()
    | x -> eprintf "qsub returned exit code %d\n" x
