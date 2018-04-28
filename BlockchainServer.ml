(* This file is in the public domain *)
open Base
open Async_kernel
open Cohttp_async

(* Source:
   https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/receive_post.ml *)

(* compile with: $ corebuild BlockchainServer.native -pkg cohttp.async *)

let start_server port () =
  Caml.Printf.eprintf "Listening for HTTP on port %d\n" port;
  Caml.Printf.eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
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

(* let () = let module Command2 = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8090 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
   |> Command.run in *)

let main () =
  Deferred.both
    (let server =
       let callback _conn req body =
         let uri = req |> Request.uri |> Uri.to_string in
         let meth = req |> Request.meth |> Code.string_of_method in
         let headers = req |> Request.headers |> Header.to_string in
         body |> Cohttp_lwt.Body.to_string >|= (fun body ->
             (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
                uri meth headers body))
         >>= (fun body -> Server.respond_string ~status:`OK ~body ())
       in
       Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ()))
    (let () = Stdio.print_endline "yay!" in return ())


let () =
  don't_wait_for (main () >>= fun _ -> exit 0);
  ignore (Async.Scheduler.go ())
(*
let () =
  let module Command = Async_extra.Command in
  let () =
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run *)
