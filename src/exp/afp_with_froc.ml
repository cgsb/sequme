
(*

    ocamlfind ocamlc -package froc,core -thread -linkpkg src/exp/afp_with_froc.ml -o awf && awf


C.f. <http://jaked.github.io/froc/doc/Froc_sa.html>

*)


open Core.Std

let test0 () =
  Froc_sa.init ();
  Froc_sa.set_debug (fun s -> eprintf "FROC: %s\n%!" s);
  let dbg fmt =
    eprintf ("DBG: " ^^ fmt ^^ "\n%!") in
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
  Froc_sa.write input_writable 51;
  dbg "after write: %d" Froc_sa.(read computation);
  Froc_sa.propagate ();
  dbg "after propagate: %d" Froc_sa.(read computation);
  ()


let () =
  test0 ()
