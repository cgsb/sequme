open Sequme_flow

let max_message_length = 10_000_000
  
let bin_send oc msg =
  if String.length msg > max_message_length then
    error (`bin_send (`message_too_long msg))
  else
    wrap_io ~on_exn:(fun e -> `bin_send (`error e))
      Lwt.(fun s ->
        Lwt_io.BE.write_int oc (String.length s) >>= fun () ->
        Lwt_io.write oc s)
      msg

let bin_recv ic =
  wrap_io ~on_exn:(fun e -> `bin_recv (`error e))
    Lwt.(fun () ->
      Lwt_io.BE.read_int ic >>= fun c ->
      begin if max_message_length >= c && c > 0 then (
        let s = String.make c 'B' in
        Lwt_io.read_into_exactly ic s 0 c >>= fun () ->
        return s
      ) else
          return ""
      end
      >>= fun s ->
      return (c,s)
    ) ()
  >>= fun (c, s) ->
  if String.length s <> c then
    error (`bin_recv (`wrong_length (c, s)))
  else
    return s
      
