module MysteryTokenLocker = struct 
type token_id = nat
type token_type = Fa12 of address | Fa2 of address * token_id | Tez of bool

type ledger = ((address * nat), nat) big_map
type fa12_transfer = [@layout:comb] { from : address; [@annot:to]to_: address; value: nat; }
type token_id = nat
type transfer_destination = [@layout:comb] { to_ : address; token_id : token_id; amount : nat; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_destination list; }

let mk_fa12_transfer_op (a : address) (fa12tr: fa12_transfer) = match (Tezos.get_entrypoint_opt "%transfer" a : fa12_transfer contract option) with Some c -> Tezos.transaction fa12tr 0tez c | _ -> (failwith "Invalid fa1.2 contract" : operation)
let mk_fa2_transfer_op (a:address) (fa2tr: transfer list) = match (Tezos.get_entrypoint_opt "%transfer" a : (transfer list) contract option) with Some c -> Tezos.transaction fa2tr 0tez c | _ -> (failwith "Invalid fa2 contract" : operation)

type locker = {contents: (token_type * nat) list; total_supply:nat; remaining_supply:nat; currency: token_type; base_price:nat; max_price:nat; owner:address}
type storage = { lockers:(token_id, locker) big_map; ledger: (address * token_id, nat) big_map; }
let empty_storage : storage = {lockers = (Big_map.empty : (token_id, locker) big_map); ledger = (Big_map.empty: (address * token_id, nat) big_map)}

let next_nat (n:nat) = (22695477n*n+1n) mod (Bitwise.shift_left 2n 32n)

let lock (s, (i, locker) : storage * (nat * locker)) =
    let (total, (a, txs)) : nat * (address option * transfer_destination list) = 
      List.fold_left (fun ((totalacc, (aopt, ops)), (tok_type, n) : (nat * (address option * transfer_destination list)) * (token_type * nat) ) -> 
        totalacc + n, 
        (match tok_type with 
        | Fa2(a,tid) -> 
          (match aopt with None -> Some a | Some a2 -> if a = a2 then Some a else (failwith "Tokens from only a single contract are supported" : address option )),
          ({to_ = Tezos.self_address; token_id= tid; amount = n}::ops) 
        // | Fa12 _ ->  (failwith "fa12 not an option for locking here" : address option * transfer_destination list) //(mk_fa12_transfer_op a {from = locker.owner; to_ = Tezos.self_address; value = n})::ops
        | _ -> failwith "Not implemented" : address option * transfer_destination list))
        locker.contents 
        (0n, ((None:address option ), ([]:transfer_destination list))) in
    let () = assert(total = locker.total_supply && locker.total_supply = locker.remaining_supply) in 
    [match a with Some a -> mk_fa2_transfer_op a [{from_ = Tezos.sender; txs = txs}] | _ -> (failwith "Not token transfer address" : operation)], {s with lockers = Big_map.add i locker s.lockers}

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

let redeem (s, (seed, tid, n) : storage * (nat * token_id * nat)) = 
    let rec go (seed, n, (ops,s) : nat * nat * (operation list * storage)) : operation list * storage =
      if n = 0n 
      then ops,s 
      else 
        let locker : locker =  match Big_map.find_opt tid s.lockers with | None -> (failwith "No such mystery token locker.") | Some l -> l in
        let r = seed mod (Map.size locker.contents) in 
        let ((taken, takenn), new_contents) = match Map.find_opt r locker.contents with | Some (tt,n) -> (tt,n), Map.update r (if n > 1n then Some(tt,0n) else (None : (token_type  * nat) option) ) locker.contents | _ -> (failwith "Invalid random element picked" : (token_type * nat) * (nat, (token_type * nat)) map) in
        go (
          next_nat(seed), 
          abs(n-1n), 
          ((match taken with Fa12 a -> mk_fa12_transfer_op a {from=Tezos.self_address; to_=Tezos.sender; value = takenn} | Fa2 (a,tid) -> mk_fa2_transfer_op a [{from_ = Tezos.self_address; txs = [{to_ = Tezos.sender; token_id = tid; amount = takenn}]}] | _ -> (failwith "Not implemented" : operation))::ops,
          {s with lockers = if List.length new_contents = 0n then Big_map.remove tid s.lockers else Big_map.add tid {locker with contents = new_contents} s.lockers; })
        )
      in
    go(seed, n, (([]: operation list),{ s with ledger = Big_map.update (Tezos.sender, tid) (match Big_map.find_opt (Tezos.sender, tid) s.ledger with Some n2 -> if n2 < n then (failwith "Not enough tokens." : nat option) else (if n2=n then (None: nat option) else Some(abs(n2-n))) | _ -> (None:nat option) ) s.ledger }))

let can_transfer (ts:transfer list) = Tezos.sender = Tezos.self_address || List.fold_left (fun ((b, t): bool * transfer) -> b && (Tezos.source = t.from_ || Tezos.sender = t.from_)) true ts
let transfer ((s, ts):storage * transfer list) = 
  let () = if can_transfer ts then () else failwith "FA2_NOT_OWNER" in 
  let new_ledger = 
    List.fold_left (fun ((l, t):(ledger * transfer)) -> 
      List.fold_left (fun ((l, r):ledger * transfer_destination) -> 
        match (Big_map.find_opt (t.from_, r.token_id) l, Big_map.find_opt (r.to_, r.token_id) l : (nat option * nat option)) with
        | Some n1, Some n2 -> 
          let _ = if r.amount > n1 then failwith "FA_INSUFFICIENT_BALANCE" else () in 
          let remove = Big_map.update (t.from_, r.token_id) (Some (abs(n1 - r.amount))) l in
          let add = Big_map.update (r.to_, r.token_id) (Some (r.amount + n2)) remove in
          add
        | Some n1, None -> 
          let _ = if r.amount > n1 then failwith "FA2_INSUFFICIENT_BALANCE" else () in 
          let remove = Big_map.update (t.from_, r.token_id) (Some (abs(n1 - r.amount))) l in
          let add = Big_map.update (r.to_, r.token_id) (Some (r.amount)) remove in
          add
        | None, _ -> (failwith "FA2_INSUFFICIENT_BALANCE" : ledger)
      ) l t.txs) s.ledger ts in
  ([]:operation list), {s with ledger = new_ledger}

type entrypoints = 
| Lock of nat * locker 
| Buy of (nat * nat) list
| Redeem of (nat * token_id * nat)
| [@annot:transfer] TransferMystery of transfer list
let main (e,s:entrypoints * storage) =
  match e with 
  | Lock x -> lock (s, x)
  | Buy xs -> buy (s, xs)
  | Redeem x -> redeem (s, x)
  | TransferMystery ts -> transfer(s,ts)

end