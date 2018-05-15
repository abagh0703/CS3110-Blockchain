module BlockChain :
  sig

    (* [hash] is the type for security hashes *)
    type hash = int

    (* [block] is the type for the blocks on the blockchain *)
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

    (* [blockchain] is the type for the blockchain. *)
    type blockchain = {
      chain:(block list);
      reward:float;
      bits:int;
      complexity:int;
    }

   (*Initializes an empty blockchain*)
    val empty : blockchain

    (*[make_chain] creates a blockchain based on a string [startid].*)
    val make_chain : string -> blockchain

    (* block_of_json converts json into a block type. Will raise error
     * if the json is invalid
     *)
    val block_of_json : Yojson.Basic.json -> block

    (* block_of_json converts json into a blockchain type. Will raise error
     * if the json is invalid
     *)
    val blockchain_of_json : Yojson.Basic.json -> blockchain

    (* json_of_block converts a block into json *)
    val json_of_block : block -> Yojson.json

    (* json_of_blockchain converts a blockchain into json *)
    val json_of_blockchain : blockchain -> Yojson.json

    (* block_to_string takes a block and returns a string of its
     * json representation
     *)
    val block_to_string : block -> string

    (* blockchain_to_string takes a blockchain and returns the string
     * of its json representation
     *)
    val block_chain_to_string : blockchain -> string

    (* block_printify prints a block *)
    val block_printify : block -> unit

    (* blockchain_printify prints a blockchain *)
    val blockchain_printify : blockchain -> unit

    (* hash_block returns a hash of a block object using Hashtbl.has *)
    val hash_block : block -> hash

    (* valid_block checks if the blocks information is internally valid *)
    val valid_block : block -> bool

    (* is_valid_chain checks if the chain is internally consistent *)
    val is_valid_chain : blockchain -> bool

    (* measure_complexity takes a blockchain and determines a complexity
     * based on how difficult generating such a chain would be
     *)
    val measure_complexity : blockchain -> int

    (* If adding of the block is successful, returns the new blockchain and
     * true, otherwise returns the chain unmodified and false
     *)
    val add_block : block -> blockchain -> blockchain * bool

    (*Returns true if the transaction is valid, false otherwise*)
    val check_transaction : block -> blockchain -> bool

    (*Returns the total balance in a blockchain.*)
    val check_balance : string -> float -> blockchain -> float

    (*Returns true if block is in blockchain. False other wise*)
    val in_chain : block -> blockchain -> bool

    (*Returns a block containing information about the miner*)
    val set_miner : block -> string -> block

    (*Returns a block with updated nonce value.*)
    val incr_nonce : block -> block

    (*Returns a computed nonce value.*)
    val nonnegmod : hash -> hash -> hash

   (*[sign_block] let the sender with private key [privk] sign the block [blk]*)
    val sign_block : block -> Cryptokit.RSA.key -> string -> blockchain -> block

   (*[check block] let the public check if the person who signed the block
    * is a legitimate user*)
    val check_block : Cryptokit.RSA.key -> block -> 'a -> bool

    val make_block : string -> string -> float -> string -> block
    (*[get_amount] returns the value of the block's amount.*)
    val get_amount : block -> float

    (* [get_source] returns the value of the block's source.*)
    val get_source : block -> string

    (* [set_prev_hash block chain] sets the hash of [block] on [blockchain] to
     * hash_block of [block]. *)
    val set_prev_hash : block -> blockchain -> block

  end
