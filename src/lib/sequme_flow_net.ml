open Core.Std
open Sequme_flow
open Sequme_flow_list
open Sequme_flow_sys

module Flow_ssl = struct

  let accept socket context =
    bind_on_error
      (catch_io (Lwt_ssl.ssl_accept socket) context)
      (fun e -> error (`ssl_accept_error e))


end

module Server = struct

end

module Client = struct

end
