module User :
sig

  (* [user] is a type for all users on the system, where pubk is their
   * public key and privk is their private key. *)
  type user = {pubk:string; privk:string; c:string}

(* [empty] is the empty user, used to store a value in the repl before
 * everything else happens *)
  val empty : user

(* [mine mine_mux chain_queue chain block ipsr ipm] begins the mining thread
 * for the miner who is currently runnign the repl. [mutex] is the mutex for
 * controlling mining for the user, [chain_queue] is the queue for the block
 * chain, [chain] is the blockchain, [block] is the block being mined, [ipsr]
 * is the ref to the ips list, and [ipm] is hte mutex for the [ipsr].
*)
  val mine :
    Mutex.t ->
    Crypto.BlockChain.blockchain list ref ->
    Crypto.BlockChain.blockchain ->
    Crypto.BlockChain.block ->
    string list ref -> Mutex.t -> Crypto.BlockChain.blockchain

(* [kill_mine_thread] will end the mine_thread if it is running. If it is
 * not running, then the mine thread will die when it starts running. Once the
 * thread dies once, [kill_mine_thread] needs to be called again to kill it.*)
  val kill_mine_thread : unit -> unit

(* [run_miner user mine_mux chain_queue request_mux request_queue blockchain
 *  chain_mux ipsr ipm] runs the mining function for the [user].
 *  [mine_mux] is the mutex for permitting only one miner at a time,
 *  [chain_queue] is the queue for the block chain, [chain] is the blockchain,
 *  [request_queue] is the queue of the requests for the miner to hanlde, [ipsr]
 *  is the ref to the ips list, [ipm] is hte mutex for the [ipsr], and
 *  [request_mux] is the mutex to only allow one person to access the requests
 *  at once. *)
  val run_miner :
    user * Mutex.t * Crypto.BlockChain.blockchain list ref * Mutex.t *
      Crypto.BlockChain.block list ref * Crypto.BlockChain.blockchain ref *
        Mutex.t * string list ref * Mutex.t -> 'a

  (* [new_user] creates a new record of type [user]*)
  val new_user : unit -> user

(* [unpack_user user] unpacks [user] into its three composite string values
 * (pubk privk and c) *)
  val unpack_user : user -> string * string * string

  (* [set_user pubk privk c] sets the user's pubk, privk, and c. *)
  val set_user : string -> string -> string -> user

  (* [save_user user string] saves the user in a file called [string] *)
  val save_user : user -> string -> unit

(* [make_payment_file user string] makes a payment file for user called
 * [string] *)
  val make_payment_file : user -> string -> unit

  (* [load_payment_file string] loads a payment file with name [string]*)
  val load_payment_file : string -> string

  (* [get_user string] gets a user based on the file [string].*)
  val get_user : string -> user

(* [make_transaction user dest amount chain] makes a transaction from the
 * [user]'s funds to the [dest] of value [amount] using the [chain] passed in.*)
  val make_transaction :
    user ->
    string ->
    float -> Crypto.BlockChain.blockchain -> Crypto.BlockChain.block

  end
