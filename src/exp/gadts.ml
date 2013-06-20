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

  let head : ('a, nonempty) t -> 'a =
    function
    | Cons (x, _) -> x

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
       | Cons (x, t) -> Cons (f x, non_tail_map t ~f)

   let rev_map:  type b. ('a, b) t -> f:('a -> 'c) -> ('c, b) t =
     fun l ~f ->
       match l with
       | Nil -> Nil
       | Cons (x, t) ->
         let rec revx:
           type b. ('a,  [> nonempty ]) t -> ('a, b) t -> ('c,  [> nonempty]) t =
             fun acc -> function
             | Nil -> acc
             (* | Cons (x, t) -> revx (Cons (x, (acc : ('a, [> any]) t))) t *)
             | Cons (x, t) -> revx (Cons (f x, acc)) t
         in
         begin match revx (Cons (f x, Nil)) t with
         | Nil -> assert false
         | Cons _ as c -> c
         end

   let rev = rev_map ~f:(fun x -> x)

   let map_slow l ~f = rev (rev_map l ~f)

   (*
   c.f. https://github.com/janestreet/core_kernel/blob/master/lib/core_list.ml#L381
   *)
   let rec count_map:  type b. ('a, b) t -> f:('a -> 'c) -> int -> ('c, b) t =
     fun l ~f ctr ->
       match l with
       | Nil -> Nil
       | Cons (x, Nil) -> Cons (f x, Nil)
       | Cons (x1, Cons (x2, Nil)) -> Cons (f x1, Cons (f x2, Nil))
       | Cons (x1, Cons (x2, Cons (x3, Nil))) ->
         Cons (f x1, Cons (f x2, Cons (f x3, Nil)))
       | Cons (x1, Cons (x2, Cons (x3, Cons (x4, Nil)))) ->
         Cons (f x1, Cons (f x2, Cons (f x3, Cons (f x4, Nil))))
       | Cons (x1, Cons (x2, Cons (x3, Cons (x4, Cons (x5, tl))))) ->
         let ftl =
           if ctr > 1000
           then (
             (* Printf.printf "Going map_slow ctr: %d\n%!" ctr; *)
             map_slow ~f tl
           ) else count_map ~f tl (ctr + 1) in
         Cons (f x1, Cons (f x2, Cons (f x3, Cons (f x4, Cons (f x5, ftl)))))

   let janestreet_map l ~f = count_map l ~f 0

   let map = janestreet_map

   let of_list_map l ~f = (* could be reimplemented janestreet-style *)
     let rec olm_rev acc = function
     | [] -> acc
     | h :: t -> olm_rev (Cons (f h, acc)) t
     in
     rev (olm_rev Nil l)

   let of_list = of_list_map ~f:(fun x -> x)

   let destruct:
     ('a, any) t -> empty:(unit -> 'b) -> nonempty:(('a, nonempty) t -> 'b) -> 'b
     = fun l ~empty ~nonempty ->
     match l with
     | Nil -> empty ()
     | Cons _ as ne -> nonempty ne

   let nonempty: ('a, any) t -> ('a, nonempty) t option
     = function
     | Nil -> None
     | Cons _ as ne -> Some ne

   module Unsafe = struct
     let of_list : 'a list -> ('a, any) t = Obj.magic
     let to_list : ('a, any) t -> 'a list = Obj.magic

     let self_test =
       let test initial =
         let l = of_list initial in
         let ll = map l (fun x -> x + 1) in
         let lll = List.map (fun x -> x + 1)  initial in
         assert (lll = to_list ll)
       in
       test [1;2;3;4;5;6];
       test [1;2;3;4;5;6;7;8;9;9;10;0;0;0;0;0];
       test [];
       (* The following makes `map` use `map_slow`: *)
       test (Array.to_list (Array.init 100_000 (fun x -> x)))
   end

end

open Cap_list
let e5 = of_list [1;2;3;4;5]
let e6 = map e5 ~f:(fun x -> x + 1)

module Bound_list = struct

  type zero = [ `zero ]

  type 'a successor = [ `successor of 'a ]

  type any_number = [ zero | any_number successor ]

  (* type 'a greater_than = [ `greater_than of 'a any_number ] *)

  type ('a, _) t =
  | Nil: (_, [> zero]) t
  | Cons: 'a * ('a, 'b) t -> ('a, [> 'b successor ]) t

  let head : ('a, _ successor) t -> 'a =
    function
    | Cons (x, _) -> x

  let tail : ('a, _ successor) t -> ('a, _) t =
    function
    | Cons (_, x) -> x

  let e0 = Nil
  let e1 = Cons (42, Nil)

  let e0_cast = (Nil : (int, any_number) t)
  let e1_cast = (Cons (42, Nil) : (int, any_number) t)

  let e2 = Cons (42, Cons (43, Nil))
  let e2_cast = (e2 : (int, any_number) t)

  let t1 = tail e1


  let rev1:
    ('a, any_number) t -> ('c, any_number) t = fun t ->
    let rec loop acc = function
      | Nil -> Nil
      | Cons (x, t) -> loop (Cons (x, acc)) t
    in
    loop Nil t
  (*
with

    type b. ('a, b) t -> ('c, b) t =

inference gives anyway:

    val rev1 : ('a, [> `successor of 'b | `zero ] as 'b) t -> ('a, 'b) t

which is any_number.
*)


  let rec map_non_tail:
    type b. ('a, b) t -> f:('a -> 'c) -> ('c, b) t =
    fun l ~f ->
      match l with
      | Nil -> Nil
      | Cons (x, t) -> Cons (f x, map_non_tail t ~f)

  (*
  let rev_map:  type b. ('a, b) t -> f:('a -> 'c) -> ('c, b) t =
    fun l ~f ->
      let rec revx:
        type b c. ('a,  c) t -> ('a, b) t -> ('a,  c) t =
          fun acc -> function
          | Nil -> acc
          (* | Cons (x, t) -> revx (Cons (x, (acc : ('a, [> any]) t))) t *)
          | Cons (x, t) -> revx (Cons (f x, acc)) t
      in
      revx Nil l
*)
end


(******************************************************************************)
(* Exercise 2 *)
