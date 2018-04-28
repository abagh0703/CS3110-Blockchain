(* This file is in the public domain *)
open Base
open Cohttp_async
open Mutex
open Async_kernel


(* Source:
   https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/receive_post.ml *)
(* compile with: $ corebuild bs.native -pkg cohttp.async *)

let f (r, s, m) =
  Mutex.lock m;
  r := s::!r;
  Mutex.unlock m


let start_server (r:string list ref) (m:Mutex.t) port _ =
  Caml.Printf.eprintf "Listening for HTTP on port %d\n" port;
  Caml.Printf.eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST ->
        (Body.to_string body) >>= (fun body ->
          Caml.Printf.eprintf "Body: %s" body;
          let () = ignore  (Thread.create f (r, body, m)) in
            let () = Stdio.print_endline body in
          Server.respond `OK)
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let mk_server ((r:string list ref),(m:Mutex.t)) =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) (start_server r m)
  |> Command.run; ()
