open Yojson.Basic.Util

module BlockChain = struct

  type hash = int

  type block = {
      prev_hash:hash;
      time_stamp:int;
      source:string; (*Public Key*)
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
      initial_msg:int;
    }

  let block_of_json j = {
    prev_hash= j |> member "prev_hash" |> to_int;
    time_stamp = j |> member "time_stamp" |> to_int;
    source = j |> member "source" |> to_string;
    dest = j |> member "dest" |> to_string;
    signature = j |> member "signature" |> to_string;
    nonce = j |> member "nonce" |> to_int;
    amount = j |> member "amount" |> to_float;
    complexity = j |> member "complexity" |> to_int;
    genesis = j |> member "genesis" |> to_bool;
    miner = j |> member "miner" |> to_string;
    n = j |> member "n" |> to_string;
    d = j |> member "d" |> to_string;
  }

  let blockchain_of_json j = {
    chain = j |> member "chain" |> to_list |> List.map block_of_json;
    reward = j |> member "reward" |> to_int ;
    bits = j |> member "bits" |> to_int 
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

  let declare_miner (b:block) (miner:string) =
    {b with miner = miner}

  let incr_nonce (b:block) =
    {b with nonce=b.nonce+1}

  let sign_block blk priv_key msg =
    failwith "unimplemented"

  let check_sig blk =
    failwith "unimplemnted"


end
