#use "topfind";;
#thread;;
#require "core";;
#camlp4o;;
#require "sexplib.syntax";;

open Core.Std
  
type data =
| Record of (string * data) list
| Variant of (string * data) list
| Int of int
| Float of float
| String of string
| Pointer of int
with sexp

let data_to_string d =  Sexp.(sexp_of_data d |! to_string_hum)
let data_list_to_string = function
  | [] -> "[]"
  | dl ->
    sprintf "[\n  %s;\n]" (List.map dl data_to_string |! String.concat ~sep:";\n  ")

type item = {
  id: int;
  data: data;
}
    
type data_base = {
  mutable items : item list;
  mutable last_id : int;
}

let data_base () = {items = []; last_id = 0}
  
let add_item db data =
  let id = db.last_id + 1 in
  db.last_id <- id;
  db.items <- {id; data} :: db.items;
  id
    
let print_db db =
  List.iter db.items (fun item ->
    printf "[%d] %s\n" item.id Sexp.(sexp_of_data item.data |! to_string_hum);
  );
  printf "%!"

type _ expr =
| Data: data -> data expr
| Get_pointer: data expr -> data expr
| Field: data expr * string -> data expr
| All: (data list) expr
| Has_field: string -> (data -> bool) expr
| Filter: (data list) expr * (data -> bool) expr -> (data list) expr
| Bool_function: (data -> bool) -> (data -> bool) expr

let rec expr_to_string: type t. t expr -> string = function
  | Data d -> sprintf "{%s}" (data_to_string d)
  | Get_pointer d -> sprintf "(!%s)" (expr_to_string d)
  | Field (e, s) -> sprintf "%s.%s" (expr_to_string e) s
  | All -> "ALL"
  | Has_field s -> sprintf "(has field %S?)" s
  | Filter (e1, e2) -> sprintf "(%s when %s)" (expr_to_string e1) (expr_to_string e2)
  | Bool_function _ -> sprintf "user_bool_fun"

type eval_error = [
| `wrong_pointer of int
| `no_such_field of (string * data) list * string 
| `not_a_record of data
| `not_a_pointer of data
]
with sexp

let rec eval: type t. data_base -> t expr -> (t, eval_error) Result.t = fun db e ->
  let open Result in
  match e with
  | Data d -> return d
  | Get_pointer ex ->
    eval db ex
    >>= begin function
    | Pointer p ->
      List.find db.items (fun i -> i.id = p) |! of_option ~error:(`wrong_pointer p)
    | d -> fail (`not_a_pointer d)
    end
    >>| fun i -> i.data
  | Field (ex, field_name) ->
    eval db ex
    >>= begin function
    | Record l ->
      List.Assoc.find l field_name |! of_option ~error:(`no_such_field (l, field_name))
    | d -> fail (`not_a_record d)
    end
  | All -> return (List.map db.items (fun i -> i.data))
  | Has_field (field_name) ->
    return (function
    | Record l ->
      (List.Assoc.find l field_name <> None)
    | d -> false)
  | Filter (e1, e2) ->
    eval db e1 >>= fun list_e1 ->
    eval db e2 >>= fun fun_e2 ->
    return (List.filter list_e1 fun_e2)
  | Bool_function f -> return f

(*    
#use "src/exp/adt_and_gql.ml";;
*)   

let test_expr: type t. data_base -> t expr -> (t -> string) -> unit = fun db e f ->
  printf "EXPR: %s\n" (expr_to_string e);
  begin match eval db e with
  | Ok d -> printf "OK: %s\n" (f d) 
  | Error e ->
    printf "ERROR: %s\n" Sexp.(sexp_of_eval_error e |! to_string_hum);
  end;
  printf "%!"
;;
  
let () =
  let db = data_base () in
  let id1 = add_item db (String "one first string") in
  let id2 = add_item db (Record ["name", String "The Name"; "the_string", Pointer id1]) in
  let a1 = Data (Pointer id1) in
  let a2 = Data (Pointer id2) in
  print_db db;
  test_expr db (Get_pointer a2) data_to_string;
  test_expr db (Field (Get_pointer a2, "name")) data_to_string;
  test_expr db (Get_pointer (Field (Get_pointer a2, "the_string"))) data_to_string;
  test_expr db (All) data_list_to_string;
  test_expr db (Has_field ("not_name")) (fun _ -> "is a function");
  test_expr db (Filter (All, Has_field "not_name")) data_list_to_string;
  test_expr db (Filter (All, Has_field "name")) data_list_to_string;
  test_expr db (Filter (All,
                        Bool_function (function Int 42 -> true | _ -> false))) data_list_to_string;
  ()
