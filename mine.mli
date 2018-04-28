open Crypto

(* These module will seemingly neccessarily be asynchronous to some capacity,
 * though we are not yet familiar enough to say exactly how at this time *)
module type User = sig

  (* [user] is the miners user information, stored to
   * get credit for mining blocks
  *)
  type user

  val user : user

  (* The blockchain associated with this particular user *)
  val blockchain : Blockchain.id

  (* Takes the data to be put in a block and stores it in the block *)
  val create_block : Block.data -> block

  (* Takes a block and signs it by the user user *)
  val sign_block : user -> block -> signed_block

  (* Some form of communicating a new block over the network, unsure
   * at this point of what libraries to use and how, so types may not be correct *)
  val send_block : block -> unit

  (* Gets the balance of a user, which is just the block *)
  val get_balance : block

end

module type Miner = sig

  include User

  (* The blocks data type will store the subsequent blocks to add to the chain *)
  type blocks
  (* [new_user] takes a username and password and creates a new user or
   * restores an existing user, stored with password hashed
  *)
  val create_user : string -> string -> user

  (* [mine] takes a user, an existing Blockchain, and a Signed Block and returns
   * new blockchain with the block appended.
  *)
  val mine : user -> blockchain -> signed_block -> blockchain

  (* Implements a type of FIFO queue of the blocks to be added to the chain *)
  val add_block : blocks -> block -> blocks

  (* Some form of implementing a method to listen for updates over the network,
   * unsure at this point how specifically to use our http library,
   * so types may not be correct
  *)
  val listen : unit -> unit


end

(* Further a mining repl will likely be implemented in this file *)
