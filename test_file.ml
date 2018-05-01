open Crypto.BlockChain
open User
open OUnit2
open Yojson

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
        
       
let blkchn2 = User.mine m chn_queue ch1 b1

let () = print_endline (block_chain_to_string blkchn2)
       
let () = print_int (hash_block b1)
        
   
let tests =
  [
    "trivial" >:: (fun _ -> assert_equal 1 1);
  ]

let crypto_tests =
  [
    "hash" >:: (fun _ -> assert (hash_block b1 <> hash_block b2))
  ]

let suite =
  [
    "tests" >::: tests;
    "crypto" >::: crypto_tests;
  ]

let _ = run_test_tt_main ("test suite" >::: suite)
          
