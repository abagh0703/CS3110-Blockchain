(* This file is in the public domain *)
(*open Base*)
open Cohttp_async
open Mutex
open Async_kernel
open Crypto
open Yojson



(* Source:
   https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/receive_post.ml *)
(* compile with: $ corebuild bs.native -pkg cohttp.async *)

let append_chain (r, s, m) =
  try
    let js = Yojson.Basic.from_string s in
    let ch = Crypto.BlockChain.blockchain_of_json js in
    Mutex.lock m;
    r := ch::!r;
    Mutex.unlock m
  with
    _ -> ()



let start_server_chain r (m:Mutex.t) port _ =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST ->
        (Body.to_string body) >>= (fun body ->
          Caml.Printf.eprintf "Body: %s" body;
          let () = ignore  (Thread.create append_chain (r, body, m)) in
            let () = Stdio.print_endline body in
          Server.respond_string "hi")
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let mk_server_chain (r,(m:Mutex.t)) =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) (start_server_chain r m)
  |> Command.run; ()





let append_block (r, s, m) =
  try
    let js = Yojson.Basic.from_string s in
    let ch = Crypto.BlockChain.block_of_json js in
    Mutex.lock m;
    r := ch::!r;
    Mutex.unlock m
  with
    _ -> ()



       

let start_server_block r (m:Mutex.t) chnr chnm (chnref:Crypto.BlockChain.blockchain ref) chnmux ips port _ =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
      let (uri:Uri.t) = Cohttp.Request.uri req in
      (*
      let msg = Uri.get_query_param uri "hi" in
      let () = match msg with
        | Some x -> print_endline x;
        | _ -> () in
       *)
      let pth = Uri.path uri in
      print_endline pth;
      print_endline (string_of_bool (pth = "/ips"));
      match (req |> Cohttp.Request.meth) with
      | `POST ->
         print_endline "Is posting";
         print_endline pth;
         let blk = Uri.get_query_param uri "block" in
         (match blk with
          | None -> ()
          | Some x -> ignore (Thread.create append_block (r, x, m)));
         let chn = Uri.get_query_param uri "chain" in
         (match chn with
          | None -> ()
          | Some x -> ignore (Thread.create append_chain (chnr, x, chnm)));
         if pth = "/ips" then
           (print_endline "is here";
           let nip = Uri.get_query_param uri "ip" in
           (match nip with
            | None -> ()
            | Some x -> ips := x::!ips);
           let s = List.fold_left (fun a acc -> a^"\n"^acc) "" !ips in
           Server.respond_string s)
         else
         (Mutex.lock chnmux;
            let ch = !chnref in
            Mutex.unlock chnmux;
            let txt = Crypto.BlockChain.block_chain_to_string ch in
            Server.respond_string txt)
         (*
        (Body.to_string body) >>= (fun body ->
          Caml.Printf.eprintf "Body: %s" body;
          let () = ignore  (Thread.create append_block (r, body, m)) in
          let () = Stdio.print_endline body in
          try
            let js = Yojson.Basic.from_string body in
            let () = ignore (Crypto.BlockChain.block_of_json js) in
            Server.respond_string "good"
          with
            _ -> Server.respond_string "bad")
          *)
      | `GET ->
         print_endline "Is getting????";
         if pth <> "/ips" then
           (Mutex.lock chnmux;
            let ch = !chnref in
            Mutex.unlock chnmux;
            let txt = Crypto.BlockChain.block_chain_to_string ch in
            Server.respond_string txt)

         else
           let s = List.fold_left (fun a acc -> a^"\n"^acc) "" !ips in
           Server.respond_string (s)
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let mk_server_block (r,(m:Mutex.t), chnr,chnm, chnref, chnmux, ips) =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8081 int)
                    ~doc:"int Source port to listen on"
                 ) (start_server_block r m chnr chnm chnref chnmux ips)
  |> Command.run; ()
