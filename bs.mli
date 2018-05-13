
(*Makes a blockchain for the server.*)
val mk_server_block :
  Crypto.BlockChain.block list ref * Mutex.t *
  Crypto.BlockChain.blockchain list ref * Mutex.t *
  Crypto.BlockChain.blockchain ref * Mutex.t * string list ref * Mutex.t ->
  unit

(*Terminates a block on the server*)
val kill_server_block : unit -> unit
