(* This file is in the public domain *)
open Base
open Cohttp_async
open Mutex
open Async_kernel
open Crypto
open Yojson
open Bs
open Core

(*this is for the routing table and the code is very similar to the following
 *site: https://dev.realworldocaml.org/records.html. We can talk about how to modify
 *this.
*)

type routing_info = {
  service_name: string;
  port: int;
  protocol: string
}
type 'a with_line_num = { item : 'a; line_num : int; }

(* rtmp is the routing table maintenence protocol, tcpmux is the tcpmux
 * tcp port service multiplexer. this will only work only when the access points
 * are up
*)
let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
      { item = parse line;
        line_num = line_num + 1;
      })


(*not exactly sure how to get send blockchain to multiple miners, copied
from bs.ml *)

let get_blockchain_from_miner r (m:Mutex.t) (chnref:Crypto.BlockChain.blockchain ref) chnmux port _ =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST ->
        (Body.to_string body) >>= (fun body ->
          Caml.Printf.eprintf "Body: %s" body;
          let () = ignore  (Thread.create append_block (r, body, m)) in
          let () = Stdio.print_endline body in
          try
            let js = Yojson.Basic.from_string body in
            let ch = Crypto.BlockChain.blockchain_of_json js in
            Server.respond_string "good"
          with
            _ -> Server.respond `OK)
      | `GET ->
         Mutex.lock chnmux;
         let ch = !chnref in
         Mutex.unlock chnmux;
         let txt = Crypto.BlockChain.block_chain_to_string ch in
         Server.respond_string txt
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let mk_server_block (r,(m:Mutex.t), chnref, chnmux) =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8081 int)
                    ~doc:"int Source port to listen on"
                 ) (start_server_block r m chnref chnmux)
  |> Command.run; ()
