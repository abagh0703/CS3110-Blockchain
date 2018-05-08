open Crypto.BlockChain
open User
open OUnit2
open Yojson

(* There are several paramteres that are currently hardcoded in the interest of rapid devlepment, they are listed below *)

let max_hash = 100000

(* is_valid_block is currently unimplemented, it returns true always currently for implementation purposes. Consequently, its test cases will fail *)

(* Additionally, rsa signing with cryptokit is not yet implemented, we have our own signing system as a placehold *)
   
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
       
let blkchn2 = User.mine m chn_queue ch1 b1
let b2' = List.hd blkchn2.chain

let () = print_endline (block_chain_to_string blkchn2)
       
let () = print_int (hash_block b1)

let ub = User.make_transaction u "123" 7.60 blkchn2.chain
        
let t = check_block ub blkchn2

let () = print_endline (block_to_string ub)
let () = print_endline ub.msg

let () = print_endline ""
let () = if t then print_endline "true" else print_endline "false"
       

let crypto_tests =
  [
    "hash" >:: (fun _ -> assert (hash_block b1 <> hash_block b2));
    "proper mining" >:: (fun _ -> assert (hash_block b2' < max_hash));
    "can find valid chains" >:: (fun _ -> assert (not (is_valid_chain blkchn2)));

  ]

let user_tests =
  [
    "source" >:: (fun _ -> assert_equal ub.source "7");
    "dest" >:: (fun _ -> assert_equal ub.dest "123");
    "signature" >:: (fun _ -> assert (check_block ub blkchn2));
  ]
  

let suite =
  [
    "crypto" >::: crypto_tests;
    "user" >::: user_tests;
  ]

let _ = run_test_tt_main ("test suite" >::: suite)
          
