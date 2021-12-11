
// let buy (s, xs : storage * (nat * nat) list) =
//   List.fold_left (fun (((ops, s), (tid, n)): (operation list * storage) * (token_id * nat)) -> 
//     let locker : locker =  match Big_map.find_opt tid s.lockers with | None -> (failwith "Tokens not available.") | Some l -> l in
//     let avail : nat = let n2 = locker.remaining_supply in if n2 < n || n2 = 0n then (failwith "Not enough tokens available." : nat) else n2 in
//     let total : nat = let n2 = locker.total_supply in if n2 < n || n2 = 0n then (failwith "Not enough tokens available." : nat) else n2 in
//     let circulating : nat = if total >= avail then abs(total - avail) else (failwith "Error: Total > Available" : nat) in
//     let base_price = locker.base_price in
//     let future_circulating = circulating + n in
//     //integral n=z to x of b + (n/t) * (m-b) in wolframalpha
//     let price = (n * (base_price * (2 * total - n) + locker.max_price*(circulating + future_circulating)))/(2*total) in
//     let price : nat = abs(price) in
//     (match locker.currency with Fa12 a -> mk_fa12_transfer_op a {from=Tezos.sender; to_=locker.owner; value = price} | Fa2 (a,tid) -> mk_fa2_transfer_op a [{from_ = Tezos.sender; txs = [{to_ = locker.owner; token_id = tid; amount = price}] }] | _ -> (failwith "Not Implemented" : operation) ) ::ops,
//     { 
//       s with 
//       ledger = Big_map.add (Tezos.sender, tid) (n + (match Big_map.find_opt (Tezos.sender, tid) s.ledger with Some n -> n | _ -> 0n)) s.ledger;
//       lockers = Big_map.add tid {locker with remaining_supply = abs(locker.remaining_supply - n)} s.lockers;
//     }
//   ) (([]:operation list), s) xs
type locker = {remaining_supply:nat; total_supply:nat; base_price: nat; max_price; currency:token_type; owner:address}
type storage = {lockers:(nat * locker) big_map}
let buy (s, xs : storage * (nat * nat) list) =
  List.fold_left (fun (((ops, s), (tid, n)): (operation list * storage) * (token_id * nat)) -> 
    let locker : locker =  match Big_map.find_opt tid s.lockers with | None -> (failwith "Tokens not available.") | Some l -> l in
    let avail : nat = let n2 = locker.remaining_supply in if n2 < n || n2 = 0n then (failwith "Not enough tokens available." : nat) else n2 in
    let total : nat = let n2 = locker.total_supply in if n2 < n || n2 = 0n then (failwith "Not enough tokens available." : nat) else n2 in
    let circulating : nat = if total >= avail then abs(total - avail) else (failwith "Error: Total > Available" : nat) in
    let base_price = locker.base_price in
    let future_circulating = circulating + n in
    //integral n=z to x of b + (n/t) * (m-b) in wolframalpha
    let price = (n * (base_price * (2 * total - n) + locker.max_price*(circulating + future_circulating)))/(2*total) in
    let price : nat = abs(price) in
    (match locker.currency with Fa12 a -> mk_fa12_transfer_op a {from=Tezos.sender; to_=locker.owner; value = price} | Fa2 (a,tid) -> mk_fa2_transfer_op a [{from_ = Tezos.sender; txs = [{to_ = locker.owner; token_id = tid; amount = price}] }] | _ -> (failwith "Not Implemented" : operation) ) ::ops,
    { 
      s with 
      ledger = Big_map.add (Tezos.sender, tid) (n + (match Big_map.find_opt (Tezos.sender, tid) s.ledger with Some n -> n | _ -> 0n)) s.ledger;
      lockers = Big_map.add tid {locker with remaining_supply = abs(locker.remaining_supply - n)} s.lockers;
    }
  ) (([]:operation list), s) xs