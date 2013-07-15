
(*

    ocamlfind ocamlc -package froc,core -thread -linkpkg src/exp/afp_with_froc.ml -o awf && awf


C.f. <http://jaked.github.io/froc/doc/Froc_sa.html>
also <http://jaked.github.io/froc/doc/Froc.html>

Blog post: <http://ambassadortothecomputers.blogspot.com/2010/05/how-froc-works.html>

Quick hull: <https://github.com/jaked/froc/blob/master/examples/froc-dom/quickhull/quickhull.ml>
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

  type 'a t
  val create: unit -> 'a t

  val name: string
  val of_list: 'a t -> 'a List.t -> 'a list
  val to_list: 'a list -> 'a List.t
  val qsort: 'a list -> ('a list * int)

end

module Non_adaptive_quick_sort : QSORT = struct

  type 'a list = Nil | Cons of 'a * 'a list

  type 'a t = unit
  let create () = ()

  let name = "Non_adaptive_quick_sort"
  let qsort l =
    let count_op = ref 0 in
    let filter ~f l =
      let rec faux l =
        incr count_op;
        match l with
        | Nil -> Nil
        | Cons (h, t) ->
          if f h then Cons (h, faux t) else faux t
      in
      incr count_op;
      faux l
    in
    let rec qs acc l =
      incr count_op;
      match l with
      | Nil -> acc
      | Cons (h, t) ->
        let lt = filter (fun x -> x < h) t in
        let ge = filter (fun x -> x >= h) t in
        let gs = qs acc ge in
        qs (Cons (h, gs)) lt
    in
    incr count_op;
    let result = qs Nil l in
    (result, !count_op)

  let rec of_list () = function
  | [] -> Nil
  | h :: t -> Cons (h, of_list () t)
  let rec to_list = function
  | Nil -> []
  | Cons (h, t) -> h :: (to_list t)

end

module Adaptive_quick_sort : QSORT = struct

  open Froc

  type 'a list_internal = Nil | Cons of 'a * ('a list_internal behavior)

  type 'a list = 'a list_internal behavior

  let name = "Adaptive_quick_sort"
  (*
  let changeable_list l =
    changeable ~eq:(fun l1 l2 -> l1 = Nil && l2 = Nil) l
*)

  type 'a t = {
    memo: ('a List.t -> 'a list) -> 'a List.t -> 'a list;
  }

  let id = ref 0
  let create =
    fun () ->
      incr id;
      { memo =
          memo
            ~hash:(fun l ->
                (*
                let ll = (Obj.magic l : int List.t) in
                dbg "[%d] hashing list: %s" !id
                  (List.map ll ~f:(sprintf "%d") |> String.concat ~sep:", ");
                *)
                42)
            ~eq:(fun e1 e2 -> dbg "comparing lists"; e1 = e2) () }

  let qsort l =
    let count_op = ref 0 in
    let filter ~f l =
      let rec faux chlist =
        incr count_op;
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
      incr count_op;
      faux l
    in
    let rec qs acc l =
      incr count_op;
      match l with
      | Nil -> return acc
      | Cons (h, t) ->
        filter (fun x -> x < h) t
        >>= fun lt ->
        filter (fun x -> x >= h) t
        >>= fun ge ->
        let gs = qs acc ge in
        qs (Cons (h, gs)) lt
    in
    incr count_op;
    let result = (l >>= fun ll -> qs Nil ll) in
    (result, !count_op)

  let of_list: 'a t -> 'a List.t -> 'a list =
    fun { memo } l ->
      let count_op = ref 0 in
      let rec of_list_rec = function
      | [] -> return Nil
      | h :: t ->
        incr count_op;
        let tt = memo of_list_rec  t in
        return (Cons (h, tt))
      in
      let result = memo of_list_rec l in
      dbg "of_list: %d calls" !count_op;
      result

  let rec to_list_m l =
    l >>= begin function
    | Nil -> return []
    | Cons (h, t) ->
      to_list_m t
      >>= fun l ->
      return (h :: l)
    end

  let to_list l = sample (to_list_m l)

end

let () =
  let to_string l = String.concat ~sep:", " (List.map l ~f:(sprintf "%d")) in

  Froc.init ();
  let update_event, update_event_sender =
    Froc.make_event () in
  let u = ref 0 in
  Froc.notify_e update_event (fun uu ->
      dbg "Got event %d" uu;
      u := uu + 1
    );
  let update () = Froc.send update_event_sender !u in

  dbg "nonadapt qsort: %s" Non_adaptive_quick_sort.(
      let t = create () in
      [1;3;2;4;1;5] |> of_list t |> qsort |> fst |> to_list |> to_string
    );
  dbg "adapt qsort: %s" Adaptive_quick_sort.(
      let t = create () in
      update ();
      let once =
        [1;3;2;4;1;5] |> of_list t |> qsort |> fst |> to_list |> to_string
      in
      update ();
      let twice =
        [1;3;2;4;1;5] |> of_list t |> qsort |> fst |> to_list |> to_string
      in
      update ();
      twice
    );

  let test (module Implementation : QSORT) initial_list =
    dbg "List length: %d" (List.length initial_list);
    let transformer = Implementation.create () in
    update ();
    let local_list = Implementation.of_list transformer initial_list in
    let sorted, count_op = Implementation.qsort local_list in
    dbg "%s.qsort initial: count_op: %d" Implementation.name count_op;
    update ();
    let sorted, count_op = Implementation.qsort sorted in
    dbg "%s.qsort sorted: count_op: %d" Implementation.name count_op;

    update ();
    let one_more = 500 :: 400 :: initial_list in
    let local_one_more = Implementation.of_list transformer one_more in
    let sorted, count_op = Implementation.qsort local_one_more in
    update ();
    dbg "%s.qsort one_more: count_op: %d" Implementation.name count_op;


    Implementation.to_list sorted
  in

  let bigger_list = List.init 42 (fun _ -> Random.int 1000) in
  dbg "qsort: %s" (test (module Non_adaptive_quick_sort) bigger_list |> to_string);
  dbg "qsort: %s" (test (module Adaptive_quick_sort) bigger_list |> to_string);
  ()
