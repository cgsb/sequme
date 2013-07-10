
(*

    ocamlfind ocamlc -package froc,core -thread -linkpkg src/exp/afp_with_froc.ml -o awf && awf


C.f. <http://jaked.github.io/froc/doc/Froc_sa.html>

*)


open Core.Std
let dbg fmt =
  eprintf ("DBG: " ^^ fmt ^^ "\n%!")

let test0 () =
  Froc_sa.init ();
  Froc_sa.set_debug (fun s -> eprintf "FROC: %s\n%!" s);
  let input_changeable, input_writable =
    Froc_sa.(changeable ~eq:Int.equal 42)
  in
  let computation =
    let open Froc_sa in
    input_changeable
    >>= fun v ->
    dbg "bound to input_changeable: %d" v;
    return (v + 1)
  in
  dbg "1st: %d" Froc_sa.(read computation);
  Froc_sa.write input_writable 17;
  Froc_sa.write input_writable 18;
  Froc_sa.write input_writable 51;
  dbg "after write: %d" Froc_sa.(read computation);
  Froc_sa.propagate ();
  dbg "after propagate: %d" Froc_sa.(read computation);
  ()


let () =
  test0 ()

module type QSORT = sig

  type 'a list

  val of_list: 'a List.t -> 'a list
  val to_list: 'a list -> 'a List.t
  val qsort: 'a list -> 'a list

end

module Non_adaptive_quick_sort : QSORT = struct

  type 'a list = Nil | Cons of 'a * 'a list

  let filter ~f l =
    let rec faux =
      function
      | Nil -> Nil
      | Cons (h, t) ->
        if f h then Cons (h, faux t) else faux t
    in
    faux l
  let qsort l =
    let rec qs acc = function
    | Nil -> acc
    | Cons (h, t) ->
      let lt = filter (fun x -> x < h) t in
      let ge = filter (fun x -> x >= h) t in
      let gs = qs acc ge in
      qs (Cons (h, gs)) lt
    in
    qs Nil l

  let rec of_list = function
  | [] -> Nil
  | h :: t -> Cons (h, of_list t)
  let rec to_list = function
  | Nil -> []
  | Cons (h, t) -> h :: (to_list t)

end

module Adaptive_quick_sort : QSORT = struct

  open Froc_sa

  type 'a list_internal = Nil | Cons of 'a * ('a list_internal t)

  type 'a list = 'a list_internal t

  (*
  let changeable_list l =
    changeable ~eq:(fun l1 l2 -> l1 = Nil && l2 = Nil) l
*)

  let filter ~f l =
    let rec faux chlist =
      chlist >>= fun l ->
      match l with
      (* | Nil -> write to_write Nil *)
      | Nil -> return Nil
      | Cons (h, t) ->
        if f h
        then begin
          return (Cons (h, faux t))
        end
        else
          faux t
    in
    faux l

  let qsort l =
    let rec qs acc = function
    | Nil -> return acc
    | Cons (h, t) ->
      filter (fun x -> x < h) t
      >>= fun lt ->
      filter (fun x -> x >= h) t
      >>= fun ge ->
      let gs = qs acc ge in
      qs (Cons (h, gs)) lt
    in
    l >>= fun ll ->
    qs Nil ll

  let rec of_list = function
  | [] -> return Nil
  | h :: t ->
    return (Cons (h, of_list t))
  let rec to_list_m l =
    l >>= begin function
    | Nil -> return []
    | Cons (h, t) ->
      to_list_m t
      >>= fun l ->
      return (h :: l)
    end

  let to_list l = read (to_list_m l)

end

let () =
  let to_string l = String.concat ~sep:", " (List.map l ~f:(sprintf "%d")) in

  dbg "qsort: %s" Non_adaptive_quick_sort.(
      [1;3;2;4;1;5] |> of_list |> qsort |> to_list |> to_string
    );
  dbg "qsort: %s" Adaptive_quick_sort.(
      [1;3;2;4;1;5] |> of_list |> qsort |> to_list |> to_string
    );


  ()
