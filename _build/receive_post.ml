(* This file is in the public domain *)
open Base
open Async_kernel
open Cohttp_async

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

let start_server port () =
  Stdio.print_endline "Listening for HTTP on port " ^ port in
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST ->
        (Body.to_string body) >>= (fun body ->
            Caml.Printf.eprintf "Body: %s" body;
            let () = Stdio.print_endline body in
          Server.respond `OK)
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let () =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run
