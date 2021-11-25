module MysteryTokenLocker = struct 
#include "helpers.mligo"
#include "fa2.mligo"

type locker = {contents : (token_id * nat) list; admin: address; shuffle_hash:bytes; total:nat; fa2_address:address}
type storage = { lockers:(token_id, locker) big_map; ledger: (address * token_id, nat) big_map;metadata:contract_metadata; token_metadata: token_metadata_storage}
let empty_storage : storage = {lockers = (Big_map.empty : (token_id, locker) big_map); ledger = (Big_map.empty: (address * token_id, nat) big_map); metadata = Big_map.empty:contract_metadata; token_metadata= Big_map.empty:token_metadata_storage}

let lock (s, (i, locker) : storage * (nat * locker)) =
    let () = if locker.admin = Tezos.sender then () else failwith "Locker admin different from sender." in
    let (total, txs) : nat * transfer_destination list = 
      List.fold_left (fun ((totalacc, ops), (tid, n) : (nat * transfer_destination list) * (token_id * nat) ) -> 
        totalacc + 1n, 
          ({to_ = Tezos.self_address; token_id= tid; amount = n}::ops) ) 
        (0n, ([]:transfer_destination list)) 
        locker.contents in
    let () = if total = locker.total then () else failwith "Incorrect total supply." in
    [mk_fa2_transfer_op locker.fa2_address [{from_ = Tezos.sender; txs = txs}]], 
    {s with 
      lockers = Big_map.add i locker s.lockers;
      ledger = Big_map.add (Tezos.sender, i) total s.ledger
    }
let redeem (s, (shuffled, locker_id, as) : storage * ((token_id * nat) list * token_id * address list)) = 
  let locker = match (Big_map.find_opt locker_id s.lockers : locker option) with Some l -> l | _ -> (failwith "Invalid locker." : locker) in
  let () = if locker.admin = Tezos.sender then () else failwith "No permission to redeem tokens. Must be locker admin." in
  let () = if ListHelpers.forall (fun (x:token_id * nat) -> ListHelpers.contains (fun (x:token_id * nat) (y:token_id * nat) -> x = y ) x locker.contents) shuffled then () else failwith "Not a permutation of original." in
  let () = if Crypto.blake2b (Bytes.pack shuffled) = locker.shuffle_hash then () else failwith "Shuffle does not correspond to promised hash." in
  let (total, (txs, shuffled), ledger) = List.fold_left (fun ((total, (txs, shuffled), ledger), a : (nat * (transfer_destination list * (token_id * nat) list) * ledger) * address ) -> 
    let n = match Big_map.find_opt (a, locker_id) ledger with Some n -> n | _ -> 0n in
    total+n, 
    (let taken, rem = ListHelpers.take_exact_rev_map n (fun (tid, q : token_id * nat) -> {to_ = a; token_id=tid;amount=q}) shuffled in ListHelpers.concat taken txs, rem),
    Big_map.remove (a, locker_id) ledger
    
    ) (0n, (([]:transfer_destination list), shuffled),  s.ledger) as in
  let () = if total = locker.total then () else failwith "Total supply mismatch." in
  let () = if List.length shuffled = 0n then () else failwith "Didn't redeem all tokens." in
  [mk_fa2_transfer_op locker.fa2_address [{from_ = Tezos.self_address; txs = txs}]],
  {s with lockers = Big_map.remove locker_id s.lockers; ledger = ledger}

type entrypoints = 
| Lock of nat * locker 
| Redeem of  ((token_id * nat) list * token_id * address list)
| [@annot:transfer] TransferMystery of transfer list
let main (e,s:entrypoints * storage) =
  match e with 
  | Lock x -> lock (s, x)
  | Redeem x -> redeem (s, x)
  | TransferMystery t -> let _ = if can_transfer t then () else failwith "FA2_NOT_OWNER" in let (ops, l) = transfer (s.ledger, t) in ops, {s with ledger = l}

end