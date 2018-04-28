open Queue
open Mutex
open Crypto
open Rsa

module User = struct

  type user = {hold:string}

  let get_user file_name =
    failwith "unimp"
    


  let rec mine (mine_mux:mutex) (chain_queue:Blockchain.block Queue.t) chain block =
    if Mutex.try_lock block_mux && not(Queue.is_empty block_queue) then
      let () = Mutex.unlock m in
      failwith "unimp"
    else
      let up_nonce = Blockchain.incr_nonce block in
      let (chain',added) = Blockchain.add_block up_nonce chain in
      if added then
        chain'
      else
        mine mine_mux chain_queue chain up_nonce

  let rec run_miner (u:user) mine_mux chain_queue request_mux request_queue blockchain =
    Thread.sleep 0.01;
    if Mutex.try_lock request_mux && not(Queue.is_empty request_queue) then
      let b = Queue.pop request_queue in
      if Blockchain.is_valid_block b then
        (* TODO: configure b properly *)
        let new_chain = mine mine_mux chain_queue blockchain b in
        run_miner u mine_mux chain_queue request_mux request_queue new_chain
      else
        run_miner u mine_mux chain_queue request_mux request_queue blockchain
    else
      run_miner u mine_mux chain_queue request_mux request_queue blockchain



    

    
      
    

end
  
