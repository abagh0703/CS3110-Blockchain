open Yojson.Basic.Util
open Map
open Cryptokit


module BlockChain = struct

  type hash = int



  module Chainmap = Map.Make(String)



  type block = {
      prev_hash:hash;
      time_stamp:int;
      source:string; (*Public Key*)
      dest:string;
      signature:string;
      nonce:int;
      amount:float;
      genesis:bool;
      miner:string;
      n:string;
      d:string;
      msg:string;
    }

  type blockchain = {
    chain:(block list);
    reward:float;
    bits:int;
    complexity:int;
    }


  let empty:blockchain = {
      chain = [];
      reward = 10.0;
      bits = 128;
      complexity = 1000000;
    }

  let make_chain startid =
    let blk =
      {
        prev_hash = 0;
        time_stamp = int_of_float (Unix.time ());
        source = "";
        dest = startid;
        signature = "";
        nonce = 0;
        amount = 42.;
        genesis = true;
        miner = "";
        n = "";
        d = "";
        msg = "";
      } in
    {empty with chain = [blk]}



  let block_of_json j = {
    prev_hash= j |> member "prev_hash" |> to_int;
    time_stamp = j |> member "time_stamp" |> to_int;
    source = j |> member "source" |> to_string;
    dest = j |> member "dest" |> to_string;
    signature = j |> member "signature" |> to_string;
    nonce = j |> member "nonce" |> to_int;
    amount = j |> member "amount" |> to_float;
    genesis = j |> member "genesis" |> to_bool;
    miner = j |> member "miner" |> to_string;
    n = j |> member "n" |> to_string;
    d = j |> member "d" |> to_string;
    msg = j |> member "msg" |> to_string;
  }

  let blockchain_of_json j = {
    chain = j |> member "chain" |> to_list |> List.map block_of_json;
    reward = j |> member "reward" |> to_float ;
    bits = j |> member "bits" |> to_int;
    complexity = j |> member "complexity" |> to_int;
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
      ("genesis", `Bool block.genesis);
      ("miner", `String block.miner);
      ("msg", `String block.msg);
      ("n", `String block.n);
      ("d", `String block.d);
    ]

  let json_of_blockchain blockchain =
    `Assoc [
      ("chain", `List(List.map json_of_block blockchain.chain));
      ("reward", `Float blockchain.reward);
      ("bits", `Int blockchain.bits);
      ("complexity", `Int blockchain.complexity);
    ]


  let block_to_string block =
    let j = json_of_block block in
    Yojson.to_string j

  let block_chain_to_string block_chain =
    let j = json_of_blockchain block_chain in
    Yojson.to_string j

  let block_printify block =
    print_endline ("previous block hash: "^string_of_int block.prev_hash);
    print_endline ("time stamp: "^string_of_int block.time_stamp);
    print_endline ("source: "^block.source);
    print_endline ("destination: "^block.dest);
    print_endline ("signature: "^block.signature);
    print_endline ("nonce: "^(string_of_int block.nonce));
    print_endline ("amount: "^(string_of_float block.amount));
    if block.genesis = true
    then print_endline "genesis: true"
    else print_endline "genesis: false";
    print_endline ("miner: "^block.miner);
    print_endline ("n: "^block.n);
    print_endline ("d: "^block.d);
    print_endline ("message: "^block.msg)

  let rec blockchain_printchain chain count =
    match chain with
    | [] -> print_endline "empty chain, no blocks"
    | h::t -> print_endline ("block: "^(string_of_int count));
      block_printify h;
      blockchain_printchain t (count+1)

  let blockchain_printify blockchain =
    blockchain_printchain blockchain.chain 1;
    print_endline ("reward: "^(string_of_float blockchain.reward));
    print_endline ("bits: "^(string_of_int blockchain.bits));
    print_endline ("complexity: "^(string_of_int blockchain.complexity))

  let hash_block (b:block) =
    Hashtbl.hash b

  let valid_block (b:block) =
    b.amount >= 0. && not(b.genesis)

  let valid_hash (b:block) (blks:block list) =
    match blks with
    | b'::_ ->
      hash_block b' = b.prev_hash
    | [] ->
      b.prev_hash = 0




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
    if hash_block b < ch.complexity && valid_block b then
      {ch with chain = b::ch.chain},true
    else
      ch,false


  let rec check_transaction (b:block) (ch:blockchain) =
    match (b.amount, ch.chain) with
    | (a,_) when a <= 0. -> true
    | (_,b::[]) -> false
    | (_,[]) -> false
    | (a,b'::chain') ->
       let minerew = if b'.miner = b.source then ch.reward else 0. in
       let destrew = if b'.dest = b.source then b'.amount else 0. in
       let sourcepen = if b'.source = b.source then b'.amount else 0. in
       let a' = a -. minerew -. destrew +. sourcepen in
       check_transaction {b with amount=a'} {ch with chain=chain'}

  let rec check_chain_values (ch:blockchain) (mapo:float Chainmap.t option) =
    let map = (match mapo with
      | None -> Chainmap.empty
      | Some x -> x) in
    match ch.chain with
    | b::[] ->
       let desttot = if Chainmap.mem b.source map then
                      Chainmap.find b.source map else 0. in
       let ntot = desttot +. b.amount in
       let map' = Chainmap.add b.source ntot map in

       check_chain_values {ch with chain=[]} (Some map')
    | [] ->
       Chainmap.fold (fun _ d b -> print_int (int_of_float d);(d >= 0.) && b) map true
    | b::chain' ->
       let srctot = if Chainmap.mem b.source map then
                      Chainmap.find b.source map else 0. in
       let ntot = srctot -. b.amount in
       let map' = Chainmap.add b.source ntot map in

       let mintot = if Chainmap.mem b.miner map' then
                      Chainmap.find b.miner map' else 0. in
       let nmtot = mintot +. ch.reward in
       let map2 = Chainmap.(map' |> add b.miner nmtot) in

       let desttot = if Chainmap.mem b.dest map2 then
                       Chainmap.find b.dest map2 else 0. in
       let ndtot = desttot +. b.amount in
       let map3 = Chainmap.(map2 |> add b.dest ndtot) in

       check_chain_values {ch with chain=chain'} (Some map3)


  let rec is_valid_chain (ch:blockchain) =
    match (ch.chain) with
    | b::b'::chain' ->
       if valid_block b && valid_hash b (b'::chain') && check_chain_values ch None then
        is_valid_chain {ch with chain=b'::chain'}
      else false
    | _ -> true


  let rec check_balance (id:string) (acc:float) (ch:blockchain)  =
    match ch.chain with
    | [] -> acc
    | b'::chain' ->
       let minerew = if b'.miner = id then ch.reward else 0. in
       let destrew = if b'.dest = id then b'.amount else 0. in
       let sourcepen = if b'.source = id && not (b'.genesis) then b'.amount else 0. in
       let a' = minerew +. destrew -. sourcepen in
       check_balance id (acc+.a') {ch with chain=chain'}

  let rec in_chain blk chn =
    match chn.chain with
    | [] -> false
    | b::chain' ->
       if b.source = blk.source && b.time_stamp = blk.time_stamp then
         true else in_chain blk {chn with chain = chain'}



  let set_miner (b:block) id =
    {b with miner = id}

  let incr_nonce (b:block) =
    {b with nonce = b.nonce+1}

  let nonnegmod a b =
      let c = a mod b in
      if c < 0 then
        c + b
      else
        c

  let sign_block blk key pubk blkchn =
    let block_chain = blkchn.chain in
    let b_list = List.filter (fun b -> blk.source = pubk) block_chain in
    let msg = (List.length b_list) + 2 in
    let raisepriv = Cryptokit.RSA.encrypt key (string_of_int msg) in
    {blk with signature = raisepriv; msg = string_of_int msg}

  (* *)
  let check_block key blk block_chain =
    let decryption = Cryptokit.RSA.decrypt key blk.signature in
    let length_msg = String.length blk.msg in
    let length_decryp = String.length decryption in
    let recover = String.sub decryption (length_decryp - 1 -length_msg) length_msg in
    print_endline decryption;
    (* decrypt the message *)
    if recover = blk.msg
    then true
    else false

  let make_block source dest amount n = {
      source = source;
      dest = dest;
      amount = amount;
      time_stamp = int_of_float (Unix.time ());
      nonce = 0;
      prev_hash = 0;
      miner = "";
      n = n;
      d = "0";
      genesis = false;
      signature = "";
      msg = "";
    }

  let get_amount blk = blk.amount

  let get_source blk = blk.source

  let set_prev_hash b blckchn =
    match blckchn.chain with
    | b'::_ ->
       {b with prev_hash = hash_block b'}
    | [] -> b

end
