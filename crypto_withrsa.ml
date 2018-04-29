open Yojson.Basic.Util

module BlockChain = struct

  type hash = int

  type block = {
    prev_hash:hash;
    time_stamp:Unix.tm;
    source:string; (*Public Key*)
    dest:string;
    signature:string;
    nonce:int;
    amount:float;
    complexity:int;
    genesis:bool;
    miner:string;
    msg:string;
    n:string;
    d:string;
  }

  type blockchain = {
    chain:(block list);
    reward:int;
    bits:int;
  }

  let block_of_json j = {
    prev_hash= j |> member "prev_hash" |> to_int;
    time_stamp = j |> member "time_stamp" |> Unix.gmtime;
    source = j |> member "source" |> to_string;
    dest = j |> member "dest" |> to_string;
    signature = j |> member "signature" |> to_string;
    nonce = j |> member "nonce" |> to_int;
    amount = j |> member "amount" |> to_float;
    complexity = j |> member "complexity" |> to_int;
    genesis = j |> member "genesis" |> to_float;
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

  type user = {pubk: string; privk: string; c: string}

  let sign_block blk sender privk block_chain =
    let b_list = List.filter (fun b -> blk.source = sender.pubk) block_chain in
    let msg = (List.length b_list) + 100 in
    let raisepriv = (float_of_int msg)**(float_of_string privk) in
    let sign = string_of_float raisepriv in
    {blk with signature = sign; msg = string_of_int msg}

  let check_block blk block_chain =
    let f_pubk = float_of_string blk.source in
    let f_sig = float_of_string blk.signature in
    let msg = int_of_float (f_sig**f_pubk) in
    if msg = int_of_string blk.msg
    then true
    else false
end
