module User :
sig

  type user = {pubk:string; privk:string; c:string}

  val empty : user

  val mine :
    Mutex.t ->
    Crypto.BlockChain.blockchain list ref ->
    Crypto.BlockChain.blockchain ->
    Crypto.BlockChain.block ->
    string list ref -> Mutex.t -> Crypto.BlockChain.blockchain

  val kill_mine_thread : unit -> unit

  val run_miner :
    user * Mutex.t * Crypto.BlockChain.blockchain list ref * Mutex.t *
      Crypto.BlockChain.block list ref * Crypto.BlockChain.blockchain ref *
        Mutex.t * string list ref * Mutex.t -> 'a

  val new_user : unit -> user

  val unpack_user : user -> string * string * string

  val set_user : string -> string -> string -> user

  val save_user : user -> string -> unit

  val make_payment_file : user -> string -> unit

  val load_payment_file : string -> string

  val get_user : string -> user

  val make_transaction :
    user ->
    string ->
    float -> Crypto.BlockChain.blockchain -> Crypto.BlockChain.block

  end
