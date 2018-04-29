open Queue
open Mutex
open Crypto
open Rsa
open Cryptokit
   

module User = struct

  type user = {pubk: string; privk: string; c: string}

  let get_user file_name =
    failwith "unimp"
    


  let rec mine (mine_mux:Mutex.t) (chain_queue:BlockChain.blockchain Queue.t) chain block =
    if Mutex.try_lock mine_mux then
      if not(Queue.is_empty chain_queue) then
        let poten_chain = Queue.pop chain_queue in
        let () = Mutex.unlock mine_mux in
        let c1 = BlockChain.measure_complexity chain in
        let c2 = BlockChain.measure_complexity poten_chain in
        if BlockChain.is_valid_chain poten_chain && c2 < c1 then
          poten_chain
        else
          mine mine_mux chain_queue chain block
      else
        let () = Mutex.unlock mine_mux in
        let up_nonce = BlockChain.incr_nonce block in
        let (chain',added) = BlockChain.add_block up_nonce chain in
        if added then
          chain'
        else
          mine mine_mux chain_queue chain up_nonce
    else
      let up_nonce = BlockChain.incr_nonce block in
      let (chain',added) = BlockChain.add_block up_nonce chain in
      if added then
        chain'
      else
        mine mine_mux chain_queue chain up_nonce

  let rec run_miner (u:user) mine_mux chain_queue request_mux request_queue blockchain chain_mux =
    Thread.delay 0.01;
    if Mutex.try_lock request_mux && not(Queue.is_empty request_queue) then
      let b = Queue.pop request_queue in
      if BlockChain.valid_block b then
        let b' = BlockChain.set_miner b u.pubk in
        Mutex.lock chain_mux;
        let chain' = !blockchain in
        Mutex.unlock chain_mux;
        let new_chain = mine mine_mux chain_queue chain' b' in
        Mutex.lock chain_mux;
        blockchain := new_chain;
        Mutex.unlock chain_mux;
        run_miner u mine_mux chain_queue request_mux request_queue blockchain chain_mux
      else
        run_miner u mine_mux chain_queue request_mux request_queue blockchain chain_mux
    else
      run_miner u mine_mux chain_queue request_mux request_queue blockchain chain_mux


  let new_user () =
    let key = Cryptokit.RSA.new_key 2048 in
    {pubk = key.e; privk = key.d; c = key.n}

  let unpack_user (u:user) =
    u.pubk,u.privk,u.c

  let set_user pubk privk c =
    {pubk=pubk; privk=privk; c=c}

  let make_transaction user dest amount chain =
    let block = BlockChain.make_block user.pubk dest amount in
    BlockChain.sign_block block user.pubk user.privk (int_of_string user.c) chain
    

    

end
  
