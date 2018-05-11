open Queue
open Mutex
open Crypto
open Cryptokit


module User = struct

  type user = {pubk: string; privk: string; c: string}

  let get_user file_name =
    failwith "unimp"




  (*Aram do not call this function, you got that? *)
  let rec mine (mine_mux:Mutex.t) (chain_queue:BlockChain.blockchain list ref) chain block =
    if Mutex.try_lock mine_mux then
      if !chain_queue <> [] then
        let poten_chain::remain_queue = !chain_queue in
        chain_queue := remain_queue;
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


  (* This one needs to be in its own thread, should run continuously. ALl of the mutexes, queues, and the blockchain ref must also be available to the server thread. Make sure to mutex protect everything. You need to call this function. DO you understnd Aram *)
  let rec run_miner ((u:user), mine_mux, (chain_queue:BlockChain.blockchain list ref), request_mux, (request_queue:BlockChain.block list ref), (blockchain:BlockChain.blockchain ref), (chain_mux:Mutex.t)) =
    Thread.delay 0.01;
    if Mutex.try_lock request_mux then
      if [] <> !request_queue then
        let b::remaining = !request_queue in
        request_queue := remaining;
        Mutex.unlock request_mux;
        if BlockChain.valid_block b then
          let b' = BlockChain.set_miner b u.pubk in
          Mutex.lock chain_mux;
          let chain' = !blockchain in
          Mutex.unlock chain_mux;
          let new_chain = mine mine_mux chain_queue chain' b' in
          Mutex.lock chain_mux;
          blockchain := new_chain;
          Mutex.unlock chain_mux;
          run_miner (u, mine_mux, chain_queue, request_mux, request_queue, blockchain, chain_mux)
        else
          run_miner (u, mine_mux, chain_queue, request_mux, request_queue, blockchain, chain_mux)
      else
        let () = Mutex.unlock request_mux in
        run_miner (u, mine_mux, chain_queue, request_mux, request_queue, blockchain, chain_mux)
    else
      run_miner (u, mine_mux, chain_queue, request_mux, request_queue, blockchain, chain_mux)


  (* These functions are so simple even Aram should be able to tell what they do *)
  let new_user () =
    let key = Cryptokit.RSA.new_key 128 in 
    {pubk = key.e; privk = key.d; c = key.n}

  let unpack_user (u:user) =
    u.pubk,u.privk,u.c

  let set_user pubk privk c =
    {pubk=pubk; privk=privk; c=c}

  let make_transaction user dest amount chain =
    let block = BlockChain.make_block user.pubk dest amount user.c in
    BlockChain.sign_block block user.pubk user.privk (int_of_string user.c) chain
end
