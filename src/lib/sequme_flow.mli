(** The Flow-monad core module. *)

open Core.Std

(** The flow-monad “embeds” {i Core}'s [Result]s into [Lwt] threads. *)
type ('good, 'bad) t = ('good, 'bad) Result.t Lwt.t

val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  
val return : 'a -> ('a, _) t

val (>>=) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t

(** The monadic [map] function. *)
val (>>|) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t


(** Use anything as an Error thread.  *)
val error: 'a -> ('any, 'a) t
  
(** Do something on errors (generally in order to augment them). *)
val bind_on_error: ('a, 'err) t -> f:('err -> ('a, 'b) t) -> ('a, 'b) t
    
(** Do something on both sides. *)
val double_bind: ('a, 'b) t ->
  ok:('a -> ('c, 'd) t) ->
  error:('b -> ('c, 'd) t) -> ('c, 'd) t

(** [catch_io f x] uses [Lwt.catch] to catch all the
    lwt-exceptions in the result. *)
val catch_io : f:('b -> 'a Lwt.t) -> 'b -> ('a, exn) t
  
(** Put any Lwt.t in a flow-monad like [catch_io] but put the exception
    in a polymorphic variant (the default being [`io_exn e]).  *)
val wrap_io: ?on_exn:(exn -> ([> `io_exn of exn ] as 'c)) ->
  ('a -> 'b Lwt.t) -> 'a -> ('b, 'c) t

(** [map_option] allows to put options {i inside} the t. *)
val map_option: 'a option -> f:('a -> ('c, 'b) t) -> ('c option, 'b) t 

(** [of_result] adds threading to a [Result.t]. *)
val of_result: ('a, 'b) Result.t -> ('a, 'b) t

