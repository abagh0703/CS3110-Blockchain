module BlockChain :
  sig

    type hash = int


    type block 
      
    type blockchain

       
    val empty : blockchain

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
      
    val check_transaction : block -> blockchain -> bool
      
    val check_balance : string -> float -> blockchain -> float
      
    val in_chain : block -> blockchain -> bool
      
    val set_miner : block -> string -> block
      
    val incr_nonce : block -> block
      
    val nonnegmod : hash -> hash -> hash
      
    val sign_block : block -> Cryptokit.RSA.key -> string -> blockchain -> block
      
    val check_block : Cryptokit.RSA.key -> block -> 'a -> bool
      
    val make_block : string -> string -> float -> string -> block

    val get_amount : block -> float

    val get_source : block -> string
      
  end
