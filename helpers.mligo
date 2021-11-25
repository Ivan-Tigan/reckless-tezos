
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

let identity (x:_a) : _a = x
let konst (a:_a) (_b:_b) : _a = a 
let item1of2 ((a,_b):_a * _b) : _a = a
let item2of2 ((_a,b):_a * _b) : _b = b

module ListHelpers = struct
    let try_item (i:nat) (xs:_a list) : _a option = let (_, res) = List.fold_left (fun (((acci, acc), x): (nat * _a option) * _a) -> acci + 1n, (if acci = i then Some x else acc)) (0n, (None: _a option)) xs in res
    let item_unsafe (i:nat) (xs:_a list) : _a option = match (try_item i xs : _a option) with Some x -> x | _ -> failwith "Index out of bounds"
    let concat (xs: _a list) (ys: _a list) : _a list = List.fold_right (fun ((x, acc): _a * _a list) -> x::acc) xs ys 
    let bind (fm: _a -> _b list) (xs: _a list) : _b list = List.fold_left (fun ((acc, x): _b list * _a) -> concat (fm x) acc) ([]: _b list) xs
    let mapi (f: nat -> _a -> _b) (xs: _a list) : _b list = let (_, res) = List.fold_right (fun ((x, (acci, acc)) : _a * (nat * _b list)) -> abs(acci-1n), (f acci x)::acc) xs (abs(List.length xs - 1n), ([]:_b list)) in res
    // let take (n:nat) 
end


// let ok (x:_a) (s: _a -> _b) (_f: string -> _b) : _b = s x 
// let err (msg:string) (_s: _a -> _b) (f: string -> _b) : _b = f msg
// let bind 
//     (x: (_x -> ((_y -> _e) -> (string -> _e) -> _e) ) -> (string -> ((_y -> _e) -> (string -> _e) -> _e)) -> ((_y -> _e) -> (string -> _e) -> _e)) 
//     (fm: _a ->((_y -> _e) -> (string -> _e) -> _e) ) 
//     : (_y -> _e) -> (string -> _e) -> _e 
//     =  
//     x (fun (okx:_x) -> fm okx) (fun (msg:string) (_s:_y -> _e) (f:string -> _e) -> f msg) 


// let x : (int -> _b) -> (string -> _b) -> _b = ok 3
// let y : (int -> _b) -> (string -> _b) -> _b = ok 4 
// let z : (int -> _b) -> (string -> _b) -> _b = err "invalid number" 


// let res (type _b) : (int -> _b) -> (string -> _b) -> _b = 
//     bind (x: ((int -> (int -> _b) -> (string -> _b) -> _b) -> (string -> (int -> _b) -> (string -> _b) -> _b) -> (int -> _b) -> (string -> _b) -> _b)) (fun (x:int) -> 
//     bind (y: ((int -> (int -> _b) -> (string -> _b) -> _b) -> (string -> (int -> _b) -> (string -> _b) -> _b) -> (int -> _b) -> (string -> _b) -> _b)) (fun (y:int) ->
//         (ok: int -> (int -> _b) -> (string -> _b) -> _b) 
//             (x * y)
//     ))

// let run_res : int = res (identity: int -> int) (fun (msg:string) -> (failwith msg : int))

// let main ((_, _s): unit * int) = ([]:operation list), run_res