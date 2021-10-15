
type token_id = nat
type transfer_destination = [@layout:comb] { to_ : address; token_id : token_id; amount : nat; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_destination list; }
type fa12_transfer = [@layout:comb] { from : address; [@annot:to]to_: address; value: nat; }
type contract_metadata = (string, bytes) big_map

type token_type = Fa12 of address | Fa2 of address * token_id | Tez of bool

let mk_fa12_transfer_op (a : address) (fa12tr: fa12_transfer) = match (Tezos.get_entrypoint_opt "%transfer" a : fa12_transfer contract option) with Some c -> Tezos.transaction fa12tr 0tez c | _ -> (failwith "Invalid fa1.2 contract" : operation)
let mk_fa2_transfer_op (a:address) (fa2tr: transfer list) = match (Tezos.get_entrypoint_opt "%transfer" a : (transfer list) contract option) with Some c -> Tezos.transaction fa2tr 0tez c | _ -> (failwith "Invalid fa2 contract" : operation)

type fees = nat * (address , nat) map 

let apply_fees_fa2 ((ts, (rem, fs)) : transfer list * fees) = 
  let fees_total = Map.fold (fun ((acc, (_,n)) : nat * (address * nat)) -> acc + n) fs rem in
  List.map (fun (tr:transfer) -> 
    { tr with 
    txs = 
      List.fold_left (fun ((acc, trd) : transfer_destination list * transfer_destination) -> 
        let (total_transfer, trds) = 
          Map.fold (fun (((accn, acc), (a,w): (nat * transfer_destination list) * (address * nat)) ) -> let n = (trd.amount * w)/fees_total in accn+n, {to_ = a; token_id = trd.token_id; amount = n}::acc)
          (Map.add trd.to_ (match Map.find_opt trd.to_ fs with Some n -> n + rem | _ -> rem) fs)
          (0n, acc) in 
        {trd with amount = if trd.amount >= total_transfer then abs(trd.amount - total_transfer) else (failwith "critical error" : nat)} :: trds 
        ) 
      ([]:transfer_destination list) 
      tr.txs 
    })
    ts

let apply_fees_fa12 ((t, (rem, fs)) : fa12_transfer * fees) =
  let fees_total = Map.fold (fun ((acc, (_,n)) : nat * (address * nat)) -> acc + n) fs rem in
  let (total_transfer, trds) = 
    Map.fold (fun (((accn, acc), (a,w): (nat * fa12_transfer list) * (address * nat)) ) -> let n = (t.value * w)/fees_total in accn+n, { t with to_ = a; value = n}::acc)
    (Map.add t.to_ (match Map.find_opt t.to_ fs with Some n -> n + rem | _ -> rem) fs)
    (0n, ([]:fa12_transfer list)) in 
  {t with value = if t.value >= total_transfer then abs(t.value - total_transfer) else (failwith "critical error" : nat)} :: trds 

type product = (token_id * nat * nat) list list 
let seller_items (p:product) = List.fold_left (fun ((acc, ps):(token_id * nat) list * (token_id * nat * nat) list) -> List.fold_left (fun ((acc, (a,b,_)): (token_id * nat) list * (token_id * nat * nat)) -> (a,b)::acc) acc ps ) ([]:(token_id * nat) list) p
type price = (address * token_type * nat)

type offer = {
  seller: address; 
  price: price; 
  product: product;
  start_: timestamp;
  end_: timestamp;
}
type auction_house = {
  counter:nat;
  price_ids: token_type set;
  offers: (nat, offer) big_map ; 
}
type storage = {
  fa2:address;
  fees:fees;
  ah:auction_house;
  metadata: contract_metadata;
}
let post ((s, post_offers):storage * offer list) = 
  let _ = List.iter (fun (o:offer) -> if o.start_ < Tezos.now || o.end_ < o.start_ then failwith "New offers must start after now and end after they have started." else ()) post_offers in
  // let _ = List.iter (fun (o:offer) -> if not (Set.mem o.price.1 s.ah.price_ids) then failwith "This is not a registered fungible currency ." else ()) post_offers in
  let _ = List.iter (fun (o:offer) -> List.iter (fun (ps : (nat*nat*nat) list) -> if 100n = List.fold_left (fun ((acc, p) : nat * (nat*nat*nat)) -> acc+p.2) 0n ps then () else failwith "Probabilities of items in each box must add up to 100.") o.product) post_offers in
  let _ = List.iter (fun (o:offer) -> List.iter (fun (ps: (nat*nat*nat) list) -> let _ = List.fold_left (fun ((accn, p): nat * (nat*nat*nat)) -> if p.2 >= accn then p.2 else (failwith "Probabilities of items in each box must be in ascending order.":nat) ) 0n ps in ()) o.product) post_offers in
  let new_counter_offers = List.fold_left (fun (((i,os),o):(nat * (nat, offer) big_map) * offer) -> i+1n, Big_map.add i o os) (s.ah.counter, s.ah.offers) post_offers in 
  let new_counter = new_counter_offers.0 in 
  let new_offers = new_counter_offers.1 in
  let txs_to_lock = 
    List.fold_left (fun ((acc, o):transfer_destination list * offer) -> 
      List.fold_left (fun ((acc, (token_id, n)): transfer_destination list * (token_id * nat )) -> {to_ = Tezos.self_address; token_id = token_id; amount = n}::acc) acc (seller_items o.product)
    ) ([]:transfer_destination list) post_offers in
  let s =  {s with ah = {s.ah with offers = new_offers; counter = new_counter}} in
  [mk_fa2_transfer_op s.fa2 [{from_ = Tezos.source; txs = txs_to_lock}]], s

let bid ((s, (offer_id_amts)) : storage * (nat * nat) list) = 
  let new_offers_transfer =
    List.fold_left (fun (((acc1,acc2), (offer_id, amt)) : ((nat, offer) big_map * operation list) * (nat * nat)) ->
    match (Big_map.find_opt offer_id s.ah.offers : offer option) with
    | Some o ->
      let _ = if Tezos.source = o.seller then failwith "You cannot bid on your own items." else () in
      let _ = if o.end_ < Tezos.now then failwith "Bidding period for this auction has passed." else () in
      if amt > o.price.2 
      then 
        (Big_map.add offer_id ({o with price = (Tezos.source, o.price.1, amt)}) acc1), 
        (match o.price.1 with 
        | Fa12 a -> (mk_fa12_transfer_op a {from = Tezos.source; to_ = Tezos.self_address; value = amt})::(if o.price.0 <> o.seller then (mk_fa12_transfer_op a {from = Tezos.self_address; to_ = o.price.0; value = o.price.2})::acc2 else acc2)
        | Fa2 (a,tid) -> (mk_fa2_transfer_op a [{from_ = Tezos.source; txs = [{to_ = Tezos.self_address; token_id = tid; amount = amt}]}])::(if o.price.0 <> o.seller then (mk_fa2_transfer_op a [{from_ = Tezos.self_address; txs = [{to_ = o.price.0; token_id = tid; amount = o.price.2}]}])::acc2 else acc2)
        | _ -> failwith "Raw Tezos not implemented" : operation list)
      else (failwith "You must bid higher than the current highest bid." : (nat, offer) big_map * operation list)
    | _ -> (failwith "Invalid auction house offer id." : (nat, offer) big_map * operation list)
    ) (s.ah.offers, ([]:operation list)) offer_id_amts  in
  let s = {s with ah.offers = new_offers_transfer.0} in 
  new_offers_transfer.1, s
let next_nat (n:nat) = (22695477n*n+1n) mod (Bitwise.shift_left 2n 32n)
let finalize ((s, (seed, offer_ids)) : storage * (nat * nat list)) = 
  let new_offers_txs = 
    List.fold_left (fun (((acc1,acc2), offer_id) : ((nat, offer) big_map * operation list) * nat) -> 
      match (Big_map.find_opt offer_id acc1 : offer option) with 
      | Some o -> 
        if Tezos.now >= o.end_ 
        then
          (Big_map.update offer_id (None:offer option) acc1: (nat, offer) big_map),
          (mk_fa2_transfer_op s.fa2 [{
            from_ = Tezos.self_address; 
            txs = 
              List.fold_left (fun ((acc, p):transfer_destination list * (nat*nat*nat) list) -> 
              let sum_res = (List.fold_left (fun (((sum, acc), p): (nat*transfer_destination list) * (nat*nat*nat)) -> 
              sum + p.2, 
              {to_ = if seed mod 100n > sum + p.2 || List.length acc > 0n then o.seller else o.price.0;
              token_id = p.0; 
              amount = p.1}::acc) (0n, acc) p) in sum_res.1)
              ([]:transfer_destination list) o.product
          }])::
          (if o.price.0 <> o.seller 
          then 
            (match o.price.1 with 
            | Fa2 (a,tid) -> (mk_fa2_transfer_op a (apply_fees_fa2 ([{from_ = Tezos.self_address; txs = [{to_ = o.seller; token_id = tid; amount = o.price.2}]}], s.fees ))) :: acc2
            | Fa12 a -> List.fold_left (fun ((acc, t) : operation list * fa12_transfer) -> (mk_fa12_transfer_op a t)::acc) acc2 (apply_fees_fa12 ({from = Tezos.self_address; to_ = o.seller; value = o.price.2}, s.fees))
            | _ -> (failwith "Tezos not implemented yet" : operation list)
            )
          else acc2)
        else (failwith "Offer cannot be finalized yet." : (nat, offer) big_map * operation list)
      | _ -> (failwith "Offer does not exist" : (nat, offer) big_map * operation list)
    ) (s.ah.offers, ([]:operation list)) offer_ids in
  let s = {s with ah.offers = new_offers_txs.0} in
  new_offers_txs.1, s


type finalize_args = {seed:nat; offer_ids:nat list}
type fa2_entry_points =
  | Post of offer list
  | Bid of (nat * nat) list
  | Finalize of finalize_args
  
let main (action, s : fa2_entry_points * storage) : operation list * storage =
 match action with
 | Post t -> post (s, t)
 | Bid bs -> bid (s, bs)
 | Finalize f -> finalize (s, (f.seed, f.offer_ids))
 | _ -> failwith "Not implemented"

