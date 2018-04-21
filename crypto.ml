module BlockChain = struct

  type hash = int

  type block = {
      prev_hash:hash;
      time_stamp:Unix.tm;
      soucre:string; (*Public Key*)
      dest:string;
      signature:string;
      nonce:int;
      amount:float;
      complexity:int;
      genesis:bool;
      miner:string;
      n:string;
      d:string;


    }

  type blockchain = {
      chain:(block list);
      reward:int;
      bits:int;
    }

                  

  let hash_block (b:block) =
    Hashtbl.hash b

  let valid_block (b:block) =
    true

  let valid_hash (b:block) (blks:block list) =
    match blks with
    | b'::_ ->
       hash_block b' = b.prev_hash
    | [] ->
       b.prev_hash = 0

  let rec is_valid_chain (ch:blockchain) =
    match (ch.chain) with
    | b::chain' ->
       if valid_block b && valid_hash b chain' then
         is_valid_chain {ch with chain=chain'}
       else false
    | [] -> true


  let rec tail_complexity ch s =
    match ch.chain with
    | b::chain' ->
       tail_complexity {ch with chain=chain'} (s+b.prev_hash)
    | [] -> s

    
  let rec measure_complexity ch =
    match ch.chain with
    | b::chain' ->
       tail_complexity ch (b.prev_hash)
    | [] -> 0

  let add_block (b:block) (ch:blockchain) =
    if hash_block b < 1000000000 && valid_block b then
      {ch with chain = b::ch.chain},true
    else
      ch,false
    

  let sign_block blk =
    failwith "unimplemented"


end
