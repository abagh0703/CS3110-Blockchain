

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

(* blk_ref is the reference to the list of blocks (blockchain) *)
let blk_ref : (Crypto.BlockChain.block list) ref = ref ([])
(* blk_mux is the mutex that controls access to the blockchain *)
let blk_mux = Mutex.create ()

(* chain_ref is the reference to the blockchains *)
let chain_ref : (Crypto.BlockChain.blockchain list) ref = ref ([])
(* chain_mux is the mutex controlling access to chain_ref *)
let chain_mux = Mutex.create ()

(* blkchn is the blockchain *)
let blkchn = ref BlockChain.empty
(* blkchn_mux is the mutex controlling access to the blockchain *)
let blkchn_mux = Mutex.create ()

(* Data is all the info about the user *)
type data = {
  mutable pub_key : string;
  mutable priv_key : string;
  mutable m : string;
  mutable user : User.user;
}

(* Bad_IP is called when the user enters an invalid ip format *)
exception Bad_IP

(*
* [clean_input str] returns [str] but lowercase and trimmed
* requires: [str] is type string
*)
let clean_input str =
  str |> String.lowercase_ascii |> String.trim

(* [save_user_safely] saves user, but catches error if they enter invalid
 *   filename and makes them enter another name until a valid one is entered.
 *   Returns the safe name they ended up using.
 * Requires: [name] is a string, [u] is type User
*)
let rec save_user_safely name u =
  try
    let () = User.save_user u (name^".key") in name;
  with
  | _ -> let () = print_endline "Invalid name, please try again. Rember not to
include .key or any illegal characters." in
    let new_name = read_line() in
    save_user_safely new_name u

(*
* [input_until_safe f] runs [f] until no errors are called.
* requires: [f] is a function that requires a thunk to run
*)
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

(*
* [new_user_orientation] runs the user through all they need to know to setup
*)
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

(* [mine_thread] is the thread miners run to mine *)
let mine_thread = ref (Thread.self ())
(* [block_thread] is the thread miners run to receive and verify blocks *)
let block_thread = ref (Thread.self ())

let () = print_endline "Welcome to OCHAIN!"

(* [bad_input_message] prints whenever the user enters an invalid command *)
let bad_input_message = ref (fun () -> print_endline "Sorry, invalid command.
Please try again.")

(* [is_nums s] checks if [s] is made of 1 or more numbers and only numbers.
 *  Ignores any colons, since those are allowed in ips.
 *  Requires: [s] i s type string
*)
let rec is_nums s =
  Str.string_match (Str.regexp "[0-9]+$") s 0

(*
* [all_nums s] returns true if [s] contains a list of only numbers as strings
* requires: [s] is a list of strings
*)
let rec all_nums (s:string list) =
  match s with
  | [] -> true
  | h::t -> if is_nums h then all_nums t
    else false

(* [is_valid_ip] tests whether [ip] is in a valid ip format.
 * Requires: [ip] is type string
*)
let is_valid_ip ip =
  let parts = Str.split (Str.regexp "[^0-9]+") ip in
  (* Since regex splits on ':', make limit 5 if it has a colon and 4 if it
  doesn't.*)
  let limit =
    (try
       let () = ignore (String.index ip ':') in
       5
     with
     | _ -> 4)
  in if List.length parts <> limit then false else
    all_nums parts

(* [repl] step state runs the repl where [step] is the command to run and
 * [state] is the current state of the user.
 * Requires: [step] is type string, [state] is object of type data
*)
let rec repl step state =
  (match step with
   | "signin" ->
     let () = print_endline (
         "Would you like to be a 'miner' (use your computer's resources to mine
          for OCOINs),"^ " 'user' (send OCOINs and check your balance), or 'new'
          (a new user" ^ " to all of this)?") in
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
                             mine_thread := Thread.create User.run_miner
                                 (state.user, chain_mux, chain_ref, blk_mux,
                                  blk_ref, blkchn, blkchn_mux, ipr, ipm)) in
         print_endline "Congrats, you have just founded a brand new alt-coin.
                        You will start with 42 oCoins.";
          repl "mining" state
       | "join" ->
         let () = input_until_safe (
             fun () ->
              print_endline "Please enter your IP address:";
              let myip = read_line () in
              if is_valid_ip myip = false then raise Bad_IP else
              print_endline "Please insert a ip address to join from";
              let ip = read_line () in
              if is_valid_ip ip = false then raise Bad_IP else
              let ips = String.split_on_char '\n' (Bc.get_value (ip,"ips")) in
              ignore (List.map (fun i -> Thread.join (Thread.create post_value
                     (i,"ip",myip,"ips"))) (ips));
              let ipr = ref (myip::ips) in
              let ipm = Mutex.create () in
              blkchn := BlockChain.blockchain_of_json (Yojson.Basic.from_string
                              (get_value (ip,"")));
              block_thread := Thread.create Bs.mk_server_block (blk_ref,blk_mux,
                              chain_ref, chain_mux,blkchn, blkchn_mux, ipr, ipm);
              mine_thread := Thread.create User.run_miner (state.user, chain_mux,
                             chain_ref, blk_mux, blk_ref, blkchn, blkchn_mux,
                             ipr, ipm)) in
          repl "mining" state
       | _ ->
         let () =
           !bad_input_message()
         in repl step state)

   | "mining" ->
     let () = print_endline "You're mining. Please enter 'quit' if you want to
      quit" in
     (match read_line () |> clean_input with
      | "quit" ->
        (* Kills mine_thread *)
        let () = User.kill_mine_thread () in
        let () = Bs.kill_server_block () in
        repl "signin" state
      | _ ->
        let () = !bad_input_message() in
        repl step state)
   | "use" ->
     let () = print_endline "You either type 'balance' to check your balance,
     'send' to send OCOIN to others, or 'view' to view the blockchain" in
     (match read_line () |> clean_input with
      | "balance" ->
        let () = input_until_safe (fun () ->
            let () = print_endline "Type a blockchain ip (include the decimal
              points)" in
            let ip = read_line () |> clean_input in
            if is_valid_ip ip = false then raise Bad_IP else
              let chnstr = Bc.get_value (ip,"") in
              let chain = BlockChain.blockchain_of_json
                  (Yojson.Basic.from_string (chnstr)) in
              let bal = Crypto.BlockChain.check_balance (state.user.pubk) 0.
                  chain in
              print_endline ("The balance at ip " ^ ip ^ " is " ^
                  (string_of_float bal))) in
        repl "use" state
      | "send" ->
        let () = input_until_safe (fun () ->
            print_endline "Type in the payment file name";
            print_endline "Note: the extension .to will be added automatically";
            let name = read_line () |> clean_input in
            let dest = User.load_payment_file (name^".to") in
            print_endline "Type in an amount";
            let amnt = read_line () |> clean_input |> float_of_string in
            print_endline "Type a blockchain ip (include the decimal points)";
            let ip = read_line () |> clean_input in
            if is_valid_ip ip = false then raise Bad_IP else
              let ipstr = Bc.get_value (ip,"ips") in
              let ips = String.split_on_char '\n' (ipstr) in
              let chnstr = Bc.get_value (ip,"") in
              let chain = BlockChain.blockchain_of_json
                  (Yojson.Basic.from_string (chnstr)) in
              let blk = User.make_transaction state.user dest amnt chain in
              let jsn = Crypto.BlockChain.json_of_block blk |>
                        Yojson.to_string in
              let () = ignore (List.map (
                  fun ip -> Thread.join (Thread.create Bc.post_value
                                           (ip,"block",jsn,""))) ips)
              in print_endline ((string_of_float amnt)^
                                "coins have been sent to " ^ ip) ) in
        repl step state
      | "view" ->
        let () = input_until_safe (fun () ->
            let () = print_endline "Type a blockchain ip
                                    (include the decimal points)" in
            let ip = read_line () |> clean_input in
            if is_valid_ip ip = false then raise Bad_IP else
              let chnstr = Bc.get_value (ip,"") in
              let chain = BlockChain.blockchain_of_json
                  (Yojson.Basic.from_string (chnstr))
              in () (*TODO sabrina add print code here*)) in
        repl step state
      | _ ->
        let () = !bad_input_message() in
        repl step state)
   | _ -> failwith "Congrats, you've found a bug! Something's broken in repl.")


let () = repl "signin" {pub_key = ""; priv_key = ""; m = ""; user = User.empty}
