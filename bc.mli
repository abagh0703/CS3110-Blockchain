(*[get_value] returns a string, and allows a miner to access the blockchain,
  provided a url and pathname*)
val get_value : string * string -> string

(*[post_value] posts a blockchain to all of the miners. [key] and [value]
 identify who the miners are. *)
val post_value : string * string * string * string -> string
