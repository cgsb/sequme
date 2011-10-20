(** Module used as “Parvasive” in the library.  *)


include Batteries
include Printf


(** Utility functions. A place to quickly place functions that don't
    obviously fit into another module, or don't justify defining a new
    module. *)
module Util = struct
  exception ErrorPos of exn * int
(** An exception with a position number describing where the error
    occurred. Exact nature of the position depends on the function
    raising this exception. For example, it might be the line number
    within a file; the element number of an enum, list, or array;
    etc. *)

(** [unquote ~quote x] unquotes string [x] treating [quote] as the
    quotation character (default = '\"'. Returns original string, not
    a copy, if length of [x] < 2 or if first and last character of [x]
    are not [quote]. *)
  let unquote ?(quote='\"') (x : string) : string =
    let n = String.length x in
    if n < 2 then x
    else if x.[0] = '\"' && x.[n-1] = '\"' then
      String.slice ~first:1 ~last:(n-1) x
    else x

(** [tm_to_string tm] prints [tm] in the format "YYYY-MM-DD
    HH-MM-SS". *)
  let tm_to_string (tm : Unix.tm) : string =
    let open Unix in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + tm.tm_year) (1+tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(** [enum_errpos e] returns a new enum [e'] that behaves as [e].
    However, whenever [e] would have raised any exception [exn], [e']
    will raise [ErrorPos (exn,n)], where [n] is the element number at
    which the exception was raised. It is okay to use [e'] after an
    exception is raised, but the element number is reset to 0. *)
  let enum_errpos (e : 'a Enum.t) : 'a Enum.t =
    let n = ref (-1) in
    Enum.make
      ~next:(fun () ->
        incr n;
        let x = try Enum.get e with exn -> raise (ErrorPos(exn, !n)) in
        match x with
        | None -> raise Enum.No_more_elements
        | Some x -> x
      )
      ~count:(fun () -> Enum.count e)
      ~clone:(fun () -> Enum.clone e)


(** Like [Filename.temp_file] from Standard Library, but for
    directories. *)
  let temp_dir ?(parent_dir=Filename.temp_dir_name) ?(perm=0o700) prefix suffix =
    let prng = Random.State.make_self_init () in

  (* Equivalent to [Filename.temp_file_name] from Standard Library, but
     that library does not expose this function. *)
    let temp_name parent_dir prefix suffix =
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      Filename.concat parent_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
    in

    let rec try_name counter =
      let name = temp_name parent_dir prefix suffix in
      try Unix.mkdir name perm; name
      with e ->
        if counter >= 1000 then raise e else try_name (counter + 1)
    in try_name 0


end

