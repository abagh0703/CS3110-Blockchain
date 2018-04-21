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

  let is_valid_chain ch =
    failwith "unimplemented"

  let measure_complexity ch =
    failwith "unimplemented"

  let add_block blk ch =
    failwith "unimplemented"

  let sign_block blk =
    failwith "unimplemented"


end
