(*let () = print_endline "somethong"

  let prog1 n =
  let result = ref 0 in
  let f i =
    for j = 1 to n do
      let v = !result in
      Thread.delay (Random.float 1.);
      result := v + i;
      Printf.printf "Value %d\n" !result;

      print_int !result
    done in
  ignore (Thread.create f 1);
  Thread.create f 2

  let t2 = prog1 5


  let () = Thread.join t2
*)
  (*
let rec looper n =
  if n = 0 then () else looper (n-1)

let () = looper 5555555555

   *)


<<<<<<< HEAD
(* Compile with: ocamlbuild -use-ocamlfind -I src test.native *)
(* Run with ./test.native *)
=======
(* To compile, use: ocamlbuild -use-ocamlbuild -use-ocamlfind -I src test.native *)

>>>>>>> 0741432f3480aa035d47f0bd5aea6d03b93f222e

open Mutex
open Bs
open Mine


let r = ref ["start"]
let m = Mutex.create ()

type data = {
  mutable pub_key : string;
  mutable priv_key : string;
}

let clean_input str =
  str |> String.lowercase_ascii |> String.trim


let new_user_orientation () =
  let () = print_endline "Welcome! Here are your public and private keys. You
   will need them for everything here, so store them somewhere safe! Public key:
   TODO Private key: TODO. " (*TODO add public/privat key pair*) in
  print_endline "Now, would you like to be a 'miner' or 'user' ?
  Miners use your computer's power to earn OCOINs, while users can send OCOINs
  and check their balance."

(* Asks user for public key if they haven't already given it *)
let get_pub_key state =
  if state.pub_key = "" then
  let () = print_endline "Please paste your public key: " in
  read_line ()
  else state.pub_key

let mineThread = ref (Thread.self ())
let () = print_endline "Welcome to OCHAIN. Are you a 'miner', 'user', or
 'new'?"
(* TODO add 2nd paramter for possible options to simplify _ condition *)


let rec repl step state =
  let input = read_line () |> clean_input in
  (match step with
   | "signin" ->
     (match input with
      | "miner" -> let pkey = get_pub_key state in
        let () = state.pub_key <- pkey  in
        repl "mine" state
      | "user" -> let pkey = get_pub_key state in
        let () = state.pub_key <- pkey in
        let () = print_endline "You either type 'balance' to check your balance or 'send' to
send OCOIN to others." in repl "use" state
      | "new" -> let () = new_user_orientation () in repl "signin" state
      | _ -> let () =
               print_endline "Please enter either miner, existing user, or new
                            user"
        in repl step state)
   | "mine" -> let () = (mineThread := Thread.create Bs.mk_server (r,m))  in (*TODO can't ues Mine ? *)
let () = print_endline "You are now mining! Type 'quit' to quit at any time" in
repl "mining" state
| "mining" -> (match input with
    | "quit" -> let () = Thread.kill !mineThread in repl "miner" state
    | _ -> let () = print_endline "Please enter 'quit' if you want to quit" in
      repl step state)
| "use" -> (match input with
    | "balance" -> failwith "unimplemented"
    | "send" -> failwith "unimplemented"
    | _ -> let () = print_endline "Please enter either balance or send." in
  repl step state)
| _ -> failwith "Internal  error.")

let () = repl "signin" {pub_key = ""; priv_key = ""}



(* let point = Thread.create Bs.mk_server (r,m) *)

(* let rec f r m =
  Thread.delay 0.1;
  Mutex.lock m;
  let c = List.hd (!r) in
  Mutex.unlock m;
  print_endline (c);
  if c = "death" then () else f r m *)

(* let () = f r m *)
