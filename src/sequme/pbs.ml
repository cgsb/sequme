(** PBS support. *)
open Batteries_uni;; open Printf

exception Error of string

type mail_option = JobAborted | JobBegun | JobEnded

let mail_option_to_char = function
  | JobAborted -> 'a'
  | JobBegun -> 'b'
  | JobEnded -> 'e'

type script = {
  mail_options : mail_option list;
  user_list : string list;
  resource_list : string option;
  job_name : string option;
  priority : int option;
  stdout_path : string option;
  stderr_path : string option;
  export_qsub_env : bool;
  rerunable : bool option;
  commands : string list;
}

let make_script ?(mail_options=[]) ?(user_list=[]) ?resource_list ?job_name
    ?priority
    ?stdout_path ?stderr_path ?(export_qsub_env=false)
    ?rerunable
    commands
    =
  {
    mail_options = List.unique mail_options;
    user_list;
    resource_list;
    job_name = (match job_name with
      | None -> None
      | Some x ->
          if String.length x < 16 then Some x 
          else Error (sprintf "%s: job name must be <= 15 characters" x) |> raise
    );
    priority;
    stdout_path;
    stderr_path;
    export_qsub_env;
    rerunable;
    commands
  }

let script_to_string x =
  let e opt = sprintf "#PBS %s\n" opt in
  let s opt x = sprintf "#PBS %s %s\n" opt x in
  let i opt x = sprintf "#PBS %s %d\n" opt x in
  let header = String.concat "" [
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
