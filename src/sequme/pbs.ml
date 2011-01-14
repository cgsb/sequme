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
  stdout_path : string option;
  stderr_path : string option;
  export_qsub_env : bool;
  rerunable : bool option;
  commands : string list;
}

let make_script ?(mail_options=[]) ?(user_list=[]) ?resource_list ?job_name
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
    stdout_path;
    stderr_path;
    export_qsub_env;
    rerunable;
    commands
  }

let script_to_string x =
  let header = String.concat "" [
    (match x.mail_options with
      | [] -> ""
      | x ->
          let x = List.map (mail_option_to_char |- string_of_char) x in
          sprintf "#PBS -m %s" (String.concat "" x)
    );
    (match x.user_list with
      | [] -> ""
      | x -> sprintf "\n#PBS -M %s" (String.concat "," x)
    );
    if x.export_qsub_env then "\n#PBS -V" else "";
    (match x.rerunable with
      | None -> ""
      | Some x -> if x then "\n#PBS -r y" else "\n#PBS -r n"
    );
    (match x.resource_list with None -> "" | Some x -> sprintf "\n#PBS -l %s" x);
    (match x.stdout_path with None -> "" | Some x -> sprintf "\n#PBS -o %s" x);
    (match x.stderr_path with None -> "" | Some x -> sprintf "\n#PBS -e %s" x);
    (match x.job_name with None -> "" | Some x -> sprintf "\n#PBS -N %s" x);
  ]
  in
  header ^ "\n\n" ^ (String.concat "\n" x.commands)

let script_to_file script ?mode ?perm file : unit =
  let cout = match mode,perm with
    | None, None -> open_out file
    | Some mode, None -> open_out ~mode file
    | None, Some perm -> open_out ~perm file
    | Some mode, Some perm -> open_out ~mode ~perm file
  in
  finally (fun () -> close_out cout) (fun cout -> fprintf cout "%s\n" (script_to_string script)) cout
