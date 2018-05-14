

(* Compile with: ocamlbuild -use-ocamlfind -I src main.native *)
(* Run with ./main.native *)

open Mutex
open Bs
open User
open Yojson.Basic.Util
open Cohttp_async
open Crypto
open Bc
open Cohttp_lwt_unix
open Cohttp
open Lwt
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

(* POST example: https://github.com/mirage/ocaml-cohttp *)
(* open Lwt
open Cohttp
open Cohttp_lwt_unix

let body =
  Client.get (Uri.of_string "https://www.reddit.com/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body) *)

type data = {
  mutable pub_key : string;
  mutable priv_key : string;
  mutable m : string;
  mutable user : User.user;
}

exception Bad_IP

let clean_input str =
  str |> String.lowercase_ascii |> String.trim

(* Saves user, but catches error if they enter invalid filename and makes them
   enter another name until a valid one is entered. Returns the safe name they
   ended up using. *)
let rec save_user_safely name u =
  try
    let () = User.save_user u (name^".key") in name;
  with
  | _ -> let () = print_endline "Invalid name, please try again. Rember not to
include .key or any illegal characters." in
    let new_name = read_line() in
    save_user_safely new_name u

let rec input_until_safe f =
  try
    f()
  with
  | Bad_IP -> let () = print_endline "IPs must be of the format XXX.X[X][X].X
[X][X].X[X][X][:XXX], where X is an integer. Please try again." in
    input_until_safe f
  | _ ->
    let () = print_endline "Invalid input, please try again." in
    input_until_safe f

let new_user_orientation () =
  let u = User.new_user () in
  print_endline "What would you like to name your new key file:";
  print_endline "Note, the extension .key will be added automatically.";
  let name = (read_line () |> String.trim) in
  let safe_name = save_user_safely name u in
  User.make_payment_file u (safe_name^".to");
  print_endline ("Welcome! Your configuration file has been saved to"^name);
  print_endline "Now, would you like to be a 'miner' or 'user' ?
  Miners use your computer's power to earn OCOINs, while users can send OCOINs
  and check their balance."

let mine_thread = ref (Thread.self ())
let block_thread = ref (Thread.self ())



let () = print_endline "Welcome to OCHAIN!"

let bad_input_message = ref (fun () -> print_endline "Sorry, invalid command. Please try again.")

(* Checks if a string is made of 1 or more numbers and only numbers.
Ignores any colons, since those are allowed in ips.*)
let rec is_nums s =
  Str.string_match (Str.regexp "[0-9]+$") s 0

let rec all_nums (s:string list) =
  match s with
  | [] -> true
  | h::t -> if is_nums h then all_nums t
    else false

(* Since regex splits on ':', make limit 5 if it has a colon and 4 if it
   doesn't. *)
let is_valid_ip ip =
  let parts = Str.split (Str.regexp "[^0-9]+") ip in
  let limit =
  (try
      let () = ignore (String.index ip ':') in
    5
  with
  | _ -> 4)
  in if List.length parts <> limit then false else
  all_nums parts


let rec repl step state =
  (match step with
   | "signin" ->
     let () = print_endline (
         "Would you like to be a 'miner' (use your computer's resources to mine for OCOINs),"^
         " 'user' (send OCOINs and check your balance), or 'new' (a new user" ^
         " to all of this)?") in
     (match read_line () |> clean_input with
      | "miner" ->
         (*
        let pkey = get_pub_key state in
        let () = state.pub_key <- pkey in
        let privkey = get_priv_key state in
        let () = state.priv_key <- privkey in
        let m = get_m state in
        let () = state.m <- m in
        let user = User.set_user pkey privkey m in
          *)
         print_endline "Please enter your key file name:";
         print_endline "Note: The .key extension will be added automatically.";
         let () = input_until_safe (fun() ->
         let name = (read_line ())^".key" in
         let user = User.get_user name in
         let () = state.user <- user in
         print_endline "config done") in
         repl "mine" state
      | "user" ->
(*
         let pkey = get_pub_key state in
         let () = state.pub_key <- pkey in
         let pkey = get_pub_key state in
         let () = state.pub_key <- pkey in
         let privkey = get_priv_key state in
         let () = state.priv_key <- privkey in
         let m = get_m state in
         let () = state.m <- m in
         let user = User.set_user pkey privkey m in
 *)
         print_endline "Please enter your key file name:";
         print_endline "Note: The .key extension will be added automatically.";
         let () = input_until_safe (fun() ->
         let name = (read_line ())^".key" in
         let user = User.get_user name in
         state.user <- user) in
         repl "use" state
      | "new" ->
         let () = new_user_orientation () in repl "signin" state
      | _ ->
         let () =
           !bad_input_message()
         in repl step state)
   | "mine" ->
      (* let () = print_endline "1" in *)
      print_endline "Are you starting a new chain (new) or joining one (join)?";
      (match (read_line ()) with
       | "new" ->
         print_endline "Please enter your IP address";
         let () =
           input_until_safe (fun() ->
               let ip = read_line () in
               if is_valid_ip ip = false then raise Bad_IP else
               let startchn = BlockChain.make_chain state.user.pubk in
               blkchn := startchn;
               let ipr = ref [ip] in
          let ipm = Mutex.create () in
                             block_thread := Thread.create Bs.mk_server_block
                             (blk_ref,blk_mux, chain_ref, chain_mux,blkchn,
                             blkchn_mux, ipr, ipm);
                             mine_thread := Thread.create User.run_miner (state.user, chain_mux,
                                                                          chain_ref, blk_mux, blk_ref, blkchn, blkchn_mux, ipr, ipm)) in
         print_endline "Congrats, you have just founded a brand new alt-coin. You will start with 42 oCoins.";
          repl "mining" state
       | "join" ->
         let () = input_until_safe (fun () -> print_endline "Please enter your IP address:";
         let myip = read_line () in
         if is_valid_ip myip = false then raise Bad_IP else
          print_endline "Please insert a ip address to join from";
          let ip = read_line () in
          if is_valid_ip ip = false then raise Bad_IP else
          let ips = String.split_on_char '\n' (Bc.get_value (ip,"ips")) in
          ignore (List.map (fun i -> Thread.join (Thread.create post_value (i,"ip",myip,"ips"))) (ips));
          let ipr = ref (myip::ips) in
          let ipm = Mutex.create () in
          blkchn := BlockChain.blockchain_of_json (Yojson.Basic.from_string (get_value (ip,"")));
          block_thread := Thread.create Bs.mk_server_block (blk_ref,blk_mux, chain_ref, chain_mux,blkchn, blkchn_mux, ipr, ipm);
          mine_thread := Thread.create User.run_miner (state.user, chain_mux, chain_ref, blk_mux, blk_ref, blkchn, blkchn_mux, ipr, ipm)) in
          repl "mining" state
       | _ ->
         let () =
           !bad_input_message()
         in repl step state)
     (* print_endline "2"; *)
     (*let () = (chain_thread := Thread.create Bs.mk_server_chain (chain_ref,chain_mux)) in *)
     (* print_endline "3"; *)
     (*let () = (display_chain_thread := Thread.create *)

   | "mining" ->
     let () = print_endline "You're mining. Please enter 'quit' if you want to quit" in
     (match read_line () |> clean_input with
      | "quit" ->
        (* Kills mine_thread *)
        let () = User.kill_mine_thread () in
        let () = Bs.kill_server_block () in
        repl "signin" state
      | _ ->
        let () = print_endline "Sorry, invalid command." in
        repl step state)
   | "use" ->
     let () = print_endline "You either type 'balance' to check your balance or
     'send' to send OCOIN to others." in
     (match read_line () |> clean_input with
      | "balance" ->
        let () = print_endline "Type a blockchain ip (include the decimal points)" in
        let ip = read_line () |> clean_input in
        if is_valid_ip ip = false then raise Bad_IP else
        let chnstr = Bc.get_value (ip,"") in
        let chain = BlockChain.blockchain_of_json (Yojson.Basic.from_string (chnstr)) in
        let bal = Crypto.BlockChain.check_balance (state.user.pubk) 0. chain in (* TODO test *)
        let () = print_endline (string_of_float bal) in
        repl "use" state
      | "send" ->
         print_endline "Type in the payment file name";
         print_endline "Note: the extension .to will be added automatically";
         let name = read_line () |> clean_input in
         let dest = User.load_payment_file (name^".to") in
         print_endline "Type in an amount";
         let amnt = read_line () |> clean_input |> float_of_string in
         print_endline "Type a blockchain ip (include the decimal points)";
         let ip = read_line () |> clean_input in
         if is_valid_ip ip = false then raise Bad_IP else
         (*
        let ipstr = ref "" in
        let ipm = Mutex.create () in
          *)

         let ipstr = Bc.get_value (ip,"ips") in
         let ips = String.split_on_char '\n' (ipstr) in
         (*
        let ips = [] in
        let chnstr = ref "" in
        let chnm = Mutex.create () in
        Mutex.unlock chnm;
          *)
         let chnstr = Bc.get_value (ip,"") in
         let chain = BlockChain.blockchain_of_json (Yojson.Basic.from_string (chnstr)) in
         let blk = User.make_transaction state.user dest amnt chain in
         let jsn = Crypto.BlockChain.json_of_block blk |> Yojson.to_string in
         ignore (List.map (fun ip -> if is_valid_ip ip then Thread.join (Thread.create Bc.post_value (ip,"block",jsn,"")) else ()) ips);
         repl step state
      | _ ->
        let () = print_endline "Sorry, invalid command." in
        repl step state)
   | _ -> failwith "Internal error.")


let () = repl "signin" {pub_key = ""; priv_key = ""; m = ""; user = User.empty}

(* let point = Thread.create Bs.mk_server (r,m) *)

(* let rec f r m =
  Thread.delay 0.1;
  Mutex.lock m;
  let c = List.hd (!r) in
  Mutex.unlock m;
  print_endline (c);
  if c = "death" then () else f r m *)

(* let () = f r m *)
