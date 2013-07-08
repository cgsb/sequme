(** An attempt to port Umut Acar's code from his paper "Adaptive
    Functional Programming". This code is not functional. *)

open Core.Std

module Time = struct
  type t = Core.Std.Time.t
  let init () = raise (failwith "not implemented")
  let insert _ = raise (failwith "not implemented")

end

module type ADAPTIVE = sig
  type 'a mod
  type 'a dst
  type changeable

  val src : ('a * 'a -> bool) -> ('a dst -> changeable) -> 'a src
  val read : 'a src * ('a -> changeable) -> changeable
  val write : 'a dst * 'a -> changeable

  val init : unit -> unit
  val change : 'a src * 'a -> unit
  val propagate : unit -> unit
end

module Adapative : ADAPTIVE = struct
  type changeable = unit
  exception Unset_src

  type edge = {
    reader : unit -> unit;
    time_span : Time.t * Time.t;
  }

  type 'a node = {
    value : (unit -> 'a) ref;
    wrt : ('a -> unit) ref;
    out_edges : edge list ref;
  }

  type 'a src = 'a node
  type 'a dst = 'a node

  let current_time = ref (Time.init())

  let queue = Queue.create ()

  let init () =
    current_time := Time.init();
    queue = Queue.create ()

  let src cmp f =
    let value = ref (fun () -> raise Unset_src) in
    let wrt = ref (fun _ -> raise Unset_src) in
    let out_edges = ref [] in
    let m = {value; wrt; out_edges} in

    let change t v =
      if cmp (v, !value()) then ()
      else (
        (value := fun () -> v);
        List.iter !out_edges ~f:(fun x -> Queue.enqueue queue x);
        out_edges := []
      );
      current_time := t
    in

    let write v =
      (value := fun () -> v);
      wrt := change (Time.insert current_time)
    in

    wrt := write;
    (f m; m)

  let write ({wrt; _}, v) = !wrt v

  let read ({value; out_edges; _}, f) =
    let start = Time.insert current_time in
    let rec run () =
      f (!value ());
      out_edges := {
        reader = run;
        time_span = start, !current_time
      }::!out_edges
    in
    run()

  let change (l,v) = write(l,v)


end
