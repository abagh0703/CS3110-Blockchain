

(* Compile with: ocamlbuild -use-ocamlfind -I src main.native *)
(* Run with ./main.native *)

open Mutex
open Bs
open User
open Yojson.Basic.Util
open Cohttp_async
open Crypto
open Types
(*
type block = {
      prev_hash:int;
      time_stamp:int;
      source:string; (*Public Key*)
      dest:string;
      signature:string;
      nonce:int;
      amount:float;
      complexity:int;
      genesis:bool;
      miner:string;
      n:string;
      d:string;
      msg:string;
    }

type blockchain = {
    chain:(block list);
    reward:int;
    bits:int;
  }
 *)


let blk_ref : (Crypto.BlockChain.block list) ref = ref ([])
let blk_mux = Mutex.create ()

let chain_ref : (Crypto.BlockChain.blockchain list) ref = ref ([])
let chain_mux = Mutex.create ()

let blkchn = ref BlockChain.empty
let blkchn_mux = Mutex.create ()
               
           

type data = {
  mutable pub_key : string;
  mutable priv_key : string;
  mutable m : string;
  mutable user : User.user;
}

let clean_input str =
  str |> String.lowercase_ascii |> String.trim


let new_user_orientation () =
  let u = User.new_user () in
  let (pub,priv,m) = User.unpack_user u in
  let () = print_endline "Welcome! Here are your public and private keys. You
                          will need them for everything here, so store them somewhere safe!" in
  print_endline ("Public key: "^pub);
  print_endline ("Private key: "^priv);
  print_endline ("Modulus: "^m);
  print_endline "Now, would you like to be a 'miner' or 'user' ?
  Miners use your computer's power to earn OCOINs, while users can send OCOINs
  and check their balance."

(* Asks user for private key if they haven't already given it *)
let get_priv_key state =
  if state.priv_key = "" then
  let () = print_endline "Please paste your public key: " in
  read_line ()
  else state.priv_key

(* Asks user for public key if they haven't already given it *)
let get_pub_key state =
  if state.pub_key = "" then
  let () = print_endline "Please paste your private key: " in
  read_line ()
  else state.pub_key

let get_m state =
  if state.m = "" then
  let () = print_endline "Please paste your modulus: " in
  read_line ()
  else state.m

  
let mine_thread = ref (Thread.self ())
let chain_thread = ref (Thread.self ())
let block_thread = ref (Thread.self ())
                 
                 
                  
let () = print_endline "Welcome to OCHAIN. Are you a 'miner', 'user', or
 'new'?"
(* TODO add 2nd paramter for possible options to simplify _ condition *)


let rec repl step state =
  let input = read_line () |> clean_input in
  (match step with
   | "signin" ->
      (match input with
       | "miner" ->
          let pkey = get_pub_key state in
          let () = state.pub_key <- pkey in
          let privkey = get_priv_key state in
          let () = state.priv_key <- privkey in
          let m = get_m state in
          let () = state.m <- m in
          let user = User.set_user pkey privkey m in
          let () = state.user <- user in
          repl "mine" state
       | "user" ->
          let pkey = get_pub_key state in
          let () = state.pub_key <- pkey in
          let pkey = get_pub_key state in
          let () = state.pub_key <- pkey in
          let privkey = get_priv_key state in
          let () = state.priv_key <- privkey in
          let m = get_m state in
          let () = state.m <- m in
          let user = User.set_user pkey privkey m in
          let () = state.user <- user in
          let () = print_endline "You either type 'balance' to check your balance or 'send' to send OCOIN to others." in
          repl "use" state
       | "new" ->
          let () = new_user_orientation () in repl "signin" state
      | _ ->
         let () =
               print_endline "Please enter either miner, existing user, or new
                            user"
        in repl step state)
   | "mine" ->
      let () = (block_thread := Thread.create Bs.mk_server_block (blk_ref,blk_mux)) in (*TODO can't ues Mine ? *)
      let () = (chain_thread := Thread.create Bs.mk_server_chain (chain_ref,chain_mux)) in
      (*let () = (display_chain_thread := Thread.create *)
      let () = (mine_thread := Thread.create User.run_miner (state.user, chain_mux, chain_ref, blk_mux, blk_ref, blkchn, blkchn_mux)) in
      let () = print_endline "You are now mining! Type 'quit' to quit at any time" in
      repl "mining" state
   | "mining" ->
      (match input with
       | "quit" ->
          let () = Thread.kill !mine_thread in
          let () = Thread.kill !block_thread in
          let () = Thread.kill !chain_thread in
          repl "miner" state
       | _ ->
          let () = print_endline "Please enter 'quit' if you want to quit" in
              repl step state)
   | "use" ->
      (match input with
       | "balance" -> failwith "unimplemented"
       | "send" ->
          print_endline "Type in a destination address";
          let dest = read_line () |> clean_input in
          print_endline "Type in an amount";
          let amnt = read_line () |> clean_input |> float_of_string in
          print_endline "Type a blockchain ip (include the decimal points)";
          let ip = read_line () |> clean_input in
          let blk = Crypto.BlockChain.make_block state.pub_key dest amnt in
          let signed_blk = Crypto.BlockChain.sign_block blk state.pub_key state.priv_key (int_of_string state.m) [] in
          let jsn = Crypto.BlockChain.json_of_block signed_blk |> to_string in
          (*post ip jsn;*)
          repl step state
       | _ ->
          let () = print_endline "Please enter either balance or send." in
              repl step state)
   | _ -> failwith "Internal  error.")

let () = repl "signin" {pub_key = ""; priv_key = ""; m = ""; user = User.new_user ()}



(* let point = Thread.create Bs.mk_server (r,m) *)

(* let rec f r m =
  Thread.delay 0.1;
  Mutex.lock m;
  let c = List.hd (!r) in
  Mutex.unlock m;
  print_endline (c);
  if c = "death" then () else f r m *)

(* let () = f r m *)
