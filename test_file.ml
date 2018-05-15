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
      


let m = Mutex.create ()
             

let ufloop = User.get_user "floop.key"
let ufloop2 = User.get_user "floop2.key"

            
    

let sample_chain = Yojson.Basic.from_file "sample_chain.txt" |> blockchain_of_json
let insuf_mon = Yojson.Basic.from_file "insufficient_money.txt" |> blockchain_of_json
let bad_trans = Yojson.Basic.from_file "bad_transaction.txt" |> blockchain_of_json

let s1,s2 = match sample_chain.chain with
  | a::b::[] -> a,b
  | _ -> failwith "improper read in data"


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
          
