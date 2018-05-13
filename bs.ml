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

(*Creates a blockchain based of json data.*)
let append_chain (r, s, m) =
  try
    let js = Yojson.Basic.from_string s in
    let ch = Crypto.BlockChain.blockchain_of_json js in
    Mutex.lock m;
    r := ch::!r;
    Mutex.unlock m
  with
    _ -> ()

(*Adds a block to a given blockchain*)
let append_block (r, s, m) =
  try
    let js = Yojson.Basic.from_string s in
    let ch = Crypto.BlockChain.block_of_json js in
    Mutex.lock m;
    r := ch::!r;
    Mutex.unlock m
  with
    _ -> ()


let should_die = ref false
let kill_server_block () = should_die := true

let start_server_block r (m:Mutex.t) chnr chnm (chnref:Crypto.BlockChain.blockchain ref) chnmux ips ipm port _ =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) (fun ~body _ req ->

        let (uri:Uri.t) = Cohttp.Request.uri req in

      let pth = Uri.path uri in

      if !should_die then
        let () = Thread.exit () in
        Server.respond_string "Stopped mining" else
      match (req |> Cohttp.Request.meth) with
      | `POST ->

         let blk = Uri.get_query_param uri "block" in
         (match blk with
          | None -> ()
          | Some x -> ignore (Thread.create append_block (r, x, m)));
         let chn = Uri.get_query_param uri "chain" in
         (match chn with
          | None -> ()
          | Some x -> ignore (Thread.create append_chain (chnr, x, chnm)));
         if pth = "/ips" then
           (
            let nip = Uri.get_query_param uri "ip" in
            Mutex.lock ipm;
           (match nip with
            | None -> ()
            | Some "" -> ()
            | Some x -> ips := x::!ips);
           let s = List.fold_left (fun a acc -> a^"\n"^acc) "" !ips in
           Mutex.unlock ipm;
           Server.respond_string s)
         else
         (Mutex.lock chnmux;
            let ch = !chnref in
            Mutex.unlock chnmux;
            let txt = Crypto.BlockChain.block_chain_to_string ch in
            Server.respond_string txt)

      | `GET ->
         if pth <> "/ips" then
           (Mutex.lock chnmux;
            let ch = !chnref in
            Mutex.unlock chnmux;
            let txt = Crypto.BlockChain.block_chain_to_string ch in
            Server.respond_string txt)

         else
           let () = Mutex.lock ipm in
           let s = List.fold_left (fun a acc -> a^"\n"^acc) "" !ips in
           Mutex.unlock ipm;
           Server.respond_string (s)
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let mk_server_block (r,(m:Mutex.t), chnr,chnm, chnref, chnmux, ips, ipm) =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8081 int)
                    ~doc:"int Source port to listen on"
                 ) (start_server_block r m chnr chnm chnref chnmux ips ipm)
  |> Command.run; ()
