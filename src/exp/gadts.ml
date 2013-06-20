(******************************************************************************)
(*
  Exercise 1: Safe Tail
*)
type empty
type nonempty

type (_, _) safe_list =
| Nil  : ( _, empty) safe_list
| Cons : 'a * ('a, _) safe_list -> ('a, nonempty) safe_list

let hd : ('a, nonempty) safe_list -> 'a = function
| Cons (x, _) -> x

  (*
let tl : ('a, nonempty) safe_list -> ('a, 'b) safe_list = function
| Cons (_, x) -> x
  *)
let tl_list: ('a, nonempty) safe_list -> 'a list = function
  | Cons (_, tail) ->
    let rec to_list: type b. 'a list -> ('a, b) safe_list -> 'a list =
                       fun acc -> function
      | Nil -> acc
      | Cons (h, t) -> to_list (h :: acc) t
    in
    List.rev (to_list [] tail)

let e1 = tl_list (Cons (42, Cons (43, Cons (44, Nil))))


let rec map:
type b. ('a, b) safe_list -> f:('a -> 'c) -> ('c, b) safe_list =
  fun l ~f ->
    match l with
    | Nil -> Nil
    | Cons (x, t) -> Cons (f x, map t ~f)

      (*
let rev l =
  let rec revx: type b. ('a, b) safe_list -> ('a, b) safe_list -> ('a, b) safe_list =
      fun acc -> function
      | Nil -> acc
      | Cons (x, t) -> revx (Cons (x, acc)) t
  in
  revx Nil l
      *)
let rev: type b. ('a, b) safe_list -> ('a, b) safe_list =
  function
  | Nil -> Nil
  | Cons (x, t) ->
    let rec revx:
         type b . ('a, nonempty) safe_list -> ('a, b) safe_list -> ('c, nonempty) safe_list =
      fun acc -> function
      | Nil -> acc
      | Cons (x, t) -> revx (Cons (x, acc)) t
    in
    revx (Cons (x, Nil)) t

let nonempty_of_list: 'a list ->  ('a, nonempty) safe_list option =
  function
  | [] -> None
  | h :: t ->
    let rec aux : 'a list  -> ('a, nonempty) safe_list -> ('a, nonempty) safe_list =
      fun l acc ->
        match l with
        | [] -> acc
        | h :: t -> aux t (Cons (h, acc))
    in
    Some (aux t (Cons (h, Nil)))

module Indexed_list = struct
  type zero
  type 'a successor

  type (_, _) t =
  | Empty  : ( _, zero) t
  | Construct : 'a * ('a, 'b) t -> ('a, 'b successor) t

  let hd : ('a, 'b successor) t -> 'a = function
    | Construct (x, _) -> x

  let tl : ('a, 'b successor) t -> ('a, 'b) t = function
    | Construct (_, x) -> x

  let rec map:
  type b. ('a, b) t -> f:('a -> 'c) -> ('c, b) t =
    fun l ~f ->
      match l with
      | Empty -> Empty
      | Construct (x, t) -> Construct (f x, map t ~f)

(*
  let rev: type b. ('a, b) t -> ('a, b) t =
    function
    | Empty -> Empty
    | Construct (x, t) ->
      let rec revx:
           type b c . ('a, c successor) t -> ('a, b) t -> ('a, c successor) t =
             fun acc -> function
             | Empty -> acc
             | Construct (x, t) -> revx (Construct (x, acc)) t
      in
      revx (Construct (x, Empty)) t
*)
end


module Cap_list = struct

  type empty = [ `empty ]
  type nonempty = [ `nonempty ]
  type any = [ `empty | `nonempty ]

  type ('a, _) t =
  | Nil: (_, [> empty]) t
  | Cons: 'a * ('a, any) t -> ('a, [> nonempty ]) t

  let tail : ('a, nonempty) t -> ('a, any) t =
    function
    | Cons (_, x) -> x

  let e0 = Nil
  let e1 = Cons (42, Nil)

  let e2 = (Nil : (int, any) t)
  let e3 = (Cons (42, Nil) : (int, any) t)

  let e4 = Cons (42, Cons (43, Nil))

  let t1 = tail e1
  (* let t2 = tail e2 → Type error any ≠ nonempty *)
  (* let t3 = tail e3 → Type error any ≠ nonempty  *)
  (* let t4 = tail e0   →  type error:

     Error: This expression has type ('a, [> empty ] as 'b) t
     but an expression was expected of type ('a, nonempty) t
     Type 'b = [> `empty ] is not compatible with type
         nonempty = [ `nonempty ]
     The second variant type does not allow tag(s) `empty
  *)

  (* A `rev` that casts to any: *)
  let rev1: ('a, 'b) t -> ('a, 'b) t = fun t ->
    let rec loop acc = function
      | Nil -> Nil
      | Cons (x, t) -> loop (Cons (x, acc)) t
    in
    loop Nil t
  (* Gets inferred to:
     val rev1 : ('a, any) t -> ('a, any) t
  *)

  let t2 = rev1 e4 (* val t2 : (int, any) t *)

  (* A `rev` that uses assert false to preserve the types: *)
  let rev2: type b. ('a, b) t -> ('a, b) t =
    function
    | Nil -> Nil
    (* | (Cons (x, t) : ('a, [> nonempty]) t) -> *)
    | Cons (x, t) ->
      let rec revx:
          type b. ('a,  [> nonempty ]) t -> ('a, b) t -> ('c,  [> nonempty]) t =
             fun acc -> function
             | Nil -> acc
             (* | Cons (x, t) -> revx (Cons (x, (acc : ('a, [> any]) t))) t *)
             | Cons (x, t) -> revx (Cons (x, acc)) t
      in
      begin match revx (Cons (x, Nil)) t with
      | Nil -> assert false
      | Cons _ as c -> c
      end

   let t3 = rev2 e0
   let t4 = rev2 e4
  (*
    val rev2 : ('c, 'b) t -> ('c, 'b) t
    val t3 : ('_a, _[> empty ]) t
    val t4 : (int, _[> nonempty ]) t
  *)

   let rec non_tail_map: type b. ('a, b) t -> f:('a -> 'c) -> ('c, b) t =
     fun l ~f ->
       match l with
       | Nil -> Nil
       | Cons (x, t) -> Cons (f x, map t ~f)

end


(******************************************************************************)
(* Exercise 2 *)
