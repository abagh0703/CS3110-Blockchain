open Crypto.BlockChain
open User
open OUnit2
open Yojson
open Cryptokit


let ip = "127.0.0.1"

let uhi = User.get_user "hi.key"
        

(* Opens dummy server *)
let startchn = make_chain uhi.pubk
let chnstr = Yojson.to_string (json_of_blockchain startchn)
           
let ipr = ref [ip]
let ipm = Mutex.create ()
let blkchn_mux = Mutex.create ()
let blk_mux = Mutex.create ()
let chain_ref = ref []
let blk_ref = ref []
let chain_mux = Mutex.create ()
let blkchn = ref startchn
           
        



let _ = Thread.create Bs.mk_server_block
          (blk_ref,blk_mux, chain_ref, chain_mux,blkchn,
           blkchn_mux, ipr, ipm)

let _ = Thread.create User.run_miner (uhi, chain_mux, chain_ref, blk_mux, blk_ref, blkchn, blkchn_mux, ipr, ipm)
      

(* There are several paramteres that are currently hardcoded in the interest of rapid devlepment, they are listed below *)

let max_hash = 100000

(* is_valid_block is currently unimplemented, it returns true always currently for implementation purposes. Consequently, its test cases will fail *)

(* Additionally, rsa signing with cryptokit is not yet implemented, we have our own signing system as a placehold *)

(*
   
let st1 = Yojson.Basic.from_string "{
  \"chain\":
  [{
      \"prev_hash\": 0,
      \"time_stamp\":1524874389,
      \"source\": \"2\",
      \"dest\": \"personA\",
      \"signature\": \"36\",
      \"nonce\": 1,
      \"amount\": 3.50,
      \"complexity\": 15,
      \"genesis\": false,
      \"miner\": \"personB\",
      \"n\":\"83487343\",
      \"d\":\"837093280\",
      \"msg\":\"6\"
  }],
  \"reward\": 250,
  \"bits\": 64
}
"


let () = print_endline "1"
        
let ch1 = Crypto.BlockChain.blockchain_of_json st1

let () = print_endline "2"
        
let bj1 = Yojson.Basic.from_string "
  {
      \"prev_hash\": 0,
      \"time_stamp\":1524874389,
      \"source\": \"2\",
      \"dest\": \"personA\",
      \"signature\": \"36\",
      \"nonce\": 1,
      \"amount\": 3.50,
      \"complexity\": 15,
      \"genesis\": false,
      \"miner\": \"67987\",
      \"n\":\"83487343\",
      \"d\":\"837093280\",
      \"msg\":\"6\"
  }
"
let bj2 = Yojson.Basic.from_string "
  {
      \"prev_hash\": 45,
      \"time_stamp\":1524,
      \"source\": \"2\",
      \"dest\": \"personA\",
      \"signature\": \"36\",
      \"nonce\": 123,
      \"amount\": 4.50,
      \"complexity\": 15,
      \"genesis\": false,
      \"miner\": \"personB\",
      \"n\":\"83487343\",
      \"d\":\"837093280\",
      \"msg\":\"6\"
  }
                                    "
let b1 = block_of_json bj1
let b2 = block_of_json bj2
       


let () = print_int (hash_block b1)
let () = print_endline "" 
let () = print_int (hash_block b2)
let () = print_endline ""
let () = print_endline (block_to_string b1)

let m = Mutex.create ()

let chn_queue = ref []

let u = User.set_user "7" "23" "55"
       
let blkchn2 = User.mine m chn_queue
(*b1 (ref []) m ch1*)
let b2' = List.hd blkchn2.chain

let () = print_endline (block_chain_to_string blkchn2)
       
let () = print_int (hash_block b1)

let ub = User.make_transaction u "123" 7.60 blkchn2.chain
        
let t = check_block ub blkchn2

let () = print_endline (block_to_string ub)
let () = print_endline ub.msg

let () = print_endline ""
let () = if t then print_endline "true" else print_endline "false"

 *)

let m = Mutex.create ()
             

let ufloop = User.get_user "floop.key"
let ufloop2 = User.get_user "floop2.key"

            
    

let sample_chain = Yojson.Basic.from_file "sample_chain.txt" |> blockchain_of_json
let insuf_mon = Yojson.Basic.from_file "insufficient_money.txt" |> blockchain_of_json
let bad_trans = Yojson.Basic.from_file "bad_transaction.txt" |> blockchain_of_json

let s1::s2::[] = sample_chain.chain

let () = print_int (hash_block s2)
let () = print_endline "";print_int (hash_block s1)
         
let () = blockchain_printify sample_chain
       
              

let b1 = User.make_transaction uhi ufloop.pubk 4.5 startchn
let b2 = User.make_transaction ufloop2 ufloop.pubk 0.7 sample_chain
let b2' = List.hd (User.mine m (ref []) sample_chain b2 (ref []) m).chain

let k = RSA.new_key 128

let hikey = {k with e=uhi.pubk; d = uhi.pubk; n=uhi.c; size=128}
       
       

let crypto_tests =
  [
    "hash" >:: (fun _ -> assert (hash_block b1 <> hash_block b2));
    "proper mining" >:: (fun _ -> assert (hash_block b2' < sample_chain.complexity));
    "checks money" >:: (fun _ -> assert (not (is_valid_chain insuf_mon)));
    "check transaction" >:: (fun _ -> assert (not (is_valid_chain bad_trans)));
    "finds good chains" >:: (fun _ -> assert (is_valid_chain sample_chain));
                                      

  ]

let user_tests =
  [
    "source" >:: (fun _ -> assert_equal b1.source uhi.pubk);
    "dest" >:: (fun _ -> assert_equal b1.dest ufloop.pubk);
  ]

let post_ip () =
  ignore (Bc.post_value (ip,"ip","10.0.0.1","ips"))

let b1s = Yojson.to_string (json_of_block b1)

let () = print_endline "DO SOMETHING"
       
  
let () = print_endline uhi.pubk;print_endline (Bc.post_value (ip,"block",b1s,""))
let nchn = Bc.get_value (ip,"")

let () = print_endline "new chain"
let () = print_endline nchn

let () = Thread.delay 0.1
let nchn' = Bc.get_value (ip,"")

let () = ignore (Bc.post_value (ip,"block",Yojson.to_string (json_of_block b2),""))
let () = Thread.delay 1.1
       
          
let unchained = Bc.get_value (ip,"")


let b3 = User.make_transaction uhi ufloop.pubk 20. startchn
let () = ignore (Bc.post_value (ip,"block",Yojson.to_string (json_of_block b3),""))
let () = Thread.delay 1.1
let chain3 = Bc.get_value (ip, "")

let b4 = User.make_transaction uhi ufloop.pubk (-48.) startchn
let () = ignore (Bc.post_value (ip,"block",Yojson.to_string (json_of_block b4),""))
let () = Thread.delay 1.1
let chain4 = Bc.get_value (ip, "")
           
let b5 = User.make_transaction uhi ufloop.pubk 58. startchn
let () = ignore (Bc.post_value (ip,"block",Yojson.to_string (json_of_block b5),""))
let () = Thread.delay 1.1
let chain5 = Bc.get_value (ip, "")

let b6 = User.make_transaction ufloop ufloop2.pubk 10. startchn
let () = ignore (Bc.post_value (ip,"block",Yojson.to_string (json_of_block b6),""))
let () = Thread.delay 1.1
let chain6 = Bc.get_value (ip, "")
       
let () = print_endline nchn' 
           
let interpret_block inp b =
  let j = Yojson.Basic.from_string inp in
  let chn = blockchain_of_json j in
  match chn.chain with
  | [] -> false
  | b'::_ ->
     b'.source=b.source && b'.amount=b.amount &&
       b'.dest=b.dest
       

let network_tests = 
  [
    "add block" >:: (fun _ -> assert (not (nchn' = chnstr)));
    "check block integrity" >:: (fun _ -> assert (interpret_block nchn' b1));
    "check mining" >:: (fun _ -> assert (not (nchn = nchn')));
    "rejects wrong chain" >:: (fun _ -> assert_equal unchained nchn');
    "mining adds money" >:: (fun _ -> assert (chain3 <> unchained));
    "rejects negative money" >:: (fun _ -> assert_equal chain4 chain3);
    "rejects not enough money" >:: (fun _ -> assert_equal chain5 chain4);
    "trys another user" >:: (fun _ -> assert (chain5 <> chain6));
  ]
  

let suite =
  [
    "crypto" >::: crypto_tests;
    "user" >::: user_tests;
    "network" >::: network_tests;
  ]

let _ = run_test_tt_main ("test suite" >::: suite)
          
