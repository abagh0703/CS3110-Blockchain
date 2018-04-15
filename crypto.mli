module type Block = sig

  (* The type of data held in the block, i.e. string, int, float, etc. *)
  type data

  type block = Block

  (* The value of the data held, *)
  val data: data

  (* [timestamp] is the unix timestamp for the time that the block was created *)
  val timestamp : Unix.tm

  (* [hash] is the hash of this block, generated based off the info in here *)
  val hash : block -> string

  (* [hash_prev] is the hash of the previous block in the chain.
     If string is empty, then this is the genesis block.
  *)
  val hash_prev : string

  (* [nonce] is the nonce value, used for mining *)
  val nonce : int

  (* [genesis_b] is whether the block was the first block created *)
  val genesis_b : bool


  (* [generate_block]* takes in data for a new block and returns a new block
   * with that data
   *)
  val generate_block : data -> block


  (* [verified] stores information on the miner responsible for adding this
   block *)
  val verified : string

end

module type SignedBlock = sig

  include Block

   (*
   * [signature] is the digital signature of the user that is transferring
   * the money
   *)
  type signature = int

  type signed = SignedBlock

  (*
   * [sign_block] takes in a private key, a message, and a block and returns a
   * signed version of the same block using
   *)
  val sign_block : int -> string -> block -> signed

end


module type Blockchain = sig

  type block = Block

  type blockchain = Blockchain
  (*
   * Some sort of identification system for a specific blockchain to distinguish
   * it from another Blockchain running.
   *)
  val id : string

  (* [chain] is the list of blocks, aka the blockchain *)
  type chain = block list

  val is_valid_chain : chain -> bool

   (* [minig_reward] is the currency reward that miners get whenever they
    * mine a block.
    * Requires: int is positive
    *)
  val mining_reward : int

  (*
   * [add_block] block int block returns the blockchain with the block added
   * int is the difficulty, used to determine how long mining will take.
   * The higher difficulty, the longer mining will take.
  *)
  val add_block : block -> blockchain -> blockchain

  (*
   * [find_block] block int block returns the block in the [blockchain]
   * given its hash result.
   * requires: [string] is a legitimate hash result
  *)
  val find_block : string -> blockchain -> block

end
