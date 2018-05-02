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
      msg:string;
    }

  type blockchain = {
    chain:(block list);
    reward:int;
    bits:int;
    }


  let empty:blockchain = {
      chain = [];
      reward = 10;
      bits = 2048
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
    msg = j |> member "msg" |> to_string;
  }

  let blockchain_of_json j = {
    chain = j |> member "chain" |> to_list |> List.map block_of_json;
    reward = j |> member "reward" |> to_int ;
    bits = j |> member "bits" |> to_int
  }

  let json_of_block block =
    `Assoc[
      ("prev_hash", `Int block.prev_hash);
      ("time_stamp", `Int block.time_stamp);
      ("source", `String block.source);
      ("dest", `String block.dest);
      ("signature", `String block.signature);
      ("nonce", `Int block.nonce);
      ("amount", `Float block.amount);
      ("complexity", `Int block.complexity);
      ("genesis", `Bool block.genesis);
      ("miner", `String block.miner);
      ("msg", `String block.msg);
      ("n", `String block.n);
      ("d", `String block.d);
    ]

  let json_of_blockchain blockchain =
    `Assoc [
      ("chain", `List(List.map json_of_block blockchain.chain));
      ("reward", `Int blockchain.reward);
      ("bits", `Int blockchain.bits)
    ]


  let block_to_string block =
    let j = json_of_block block in
    Yojson.to_string j

  let block_chain_to_string block_chain =
    let j = json_of_blockchain block_chain in
    Yojson.to_string j


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
    if hash_block b < 100000 && valid_block b then
      {ch with chain = b::ch.chain},true
    else
      ch,false

  let set_miner (b:block) id =
    {b with miner = id}

  let incr_nonce (b:block) =
    {b with nonce = b.nonce+1}


  (*let sign_block blk priv_key msg =
    failwith "unimplemented"
  let check_sig blk =
    failwith "unimplemnted"*)


  type user = {pubk: string; privk: string; c: string}

  let nonnegmod a b =
      let c = a mod b in
      if c < 0 then
        c + b
      else
        c

  (* [sign_block] let the sender with private key [privk] sign the block
   * [blk] *)
    let sign_block blk pubk privk c block_chain =
      let b_list = List.filter (fun b -> blk.source = pubk) block_chain in
      let msg = (List.length b_list) + 2 in
      let raisepriv = int_of_float(((float_of_int msg)**(float_of_string privk))) in
      let sgn = nonnegmod raisepriv c in
      let sign = string_of_int sgn in
      {blk with signature = sign; msg = string_of_int msg}

    
      
  (* *)
    let check_block blk block_chain =
      let f_pubk = float_of_string blk.source in
      (* get the public key *)
      let f_sig = float_of_string blk.signature in
      (* get the signiture *)
      let msg = nonnegmod (int_of_float (f_sig**f_pubk)) (int_of_string blk.n) in
      (* decrypt the message *)
      if msg = int_of_string blk.msg
      then true
      else false

  let make_block source dest amount n = {
      source = source;
      dest = dest;
      amount = amount;
      time_stamp = int_of_float (Unix.time ());
      nonce = 0;
      prev_hash = 0;
      complexity = 0;
      miner = "";
      n = n;
      d = "0";
      genesis = false;
      signature = "";
      msg = "";
    }


end
