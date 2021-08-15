#if ! FA2_INTERFACE
#define FA2_INTERFACE

type token_id = nat
type transfer_destination = [@layout:comb] { to_ : address; token_id : token_id; amount : nat; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_destination list; }
type contract_metadata = (string, bytes) big_map
type price = Initial of (token_id * nat) | HighestBid of (address * token_id * nat)
#endif
type offer = {
  seller:address; 
  price: price; 
  products:(token_id * nat) list ;
  start_: timestamp;
  end_: timestamp;
}
type storage = {
  fa2:address;
  metadata: contract_metadata;
}

type fa2_entry_points =
  | Bid of (offer * nat) list
  | Post of offer list
  | Finalize of offer list
  
let main (action, s : fa2_entry_points * storage) : operation list * storage =
 match action with
 | Post t -> post (s, t)
 | _ -> failwith "Not implemented"

