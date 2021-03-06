#include "fa2.mligo"
#include "helpers.mligo"
type license_shop = { revenue_owner:address; price:(token_type, nat) map}
let get_price (ttype, x: token_type * license_shop) = (match Map.find_opt ttype x.price with Some n -> n | _ -> failwith "Invalid currency choice." : nat)

type storage = {
    nft_address: address;
    license_shop: (token_id, license_shop) big_map;
    ledger: ledger;
}

let mk_storage (nft_address:address) = {
    nft_address=nft_address;
    license_shop=(Big_map.empty: (token_id, license_shop) big_map);
    ledger=(Big_map.empty:ledger);
}
let s1 = mk_storage ("KT1PU8heQfiWgHj3Jpc1xYyE72NGGmW7L6pc":address)


type purchase_params = { tid: token_id; token_type : token_type; recipient: address }

let buy (s, p : storage * purchase_params) = 
    let _ : nat = identity 3n in 
    let shop = match Big_map.find_opt p.tid s.license_shop with Some s -> s | _ -> (failwith "Invalid license shop." : license_shop) in
    let price = get_price (p.token_type, shop) in
    let payment_tx = 
        match p.token_type with 
        | Fa2(a, tid) -> mk_fa2_transfer_op a [{from_ = p.recipient; txs=[{to_=shop.revenue_owner; token_id=tid; amount=price}]}]
        | Fa12 a -> mk_fa12_transfer_op a {from = p.recipient; to_=shop.revenue_owner;value=price}
        | Tez (_)-> if Tezos.amount/1tez = price then Tezos.transaction () Tezos.amount (match (Tezos.get_contract_opt shop.revenue_owner : unit contract option) with Some c -> c | _ -> (failwith "Invalid shop owner." : unit contract) ) else (failwith "Insufficient Tezos." : operation)
        in
    let new_ledger = BigMapHelpers.change (p.recipient, p.tid) (fun (v:nat) -> if v > 0n then (failwith "License already owned." : nat option) else Some 1n) (Some 1n) s.ledger in 
    [payment_tx], {s with ledger = new_ledger}

type modify_shop_params = {tid:token_id; shop:license_shop option}
let modify_shop (s, p: storage * modify_shop_params) = 
    let proof_own_nft = mk_fa2_transfer_op s.nft_address [{from_=Tezos.sender; txs=[{to_=Tezos.self_address; token_id=p.tid; amount=1n}]}] in
    let return_nft_proof = mk_fa2_transfer_op s.nft_address [{from_=Tezos.self_address; txs=[{to_=Tezos.sender; token_id=p.tid; amount=1n}]}] in
    [proof_own_nft; return_nft_proof],{s with license_shop = Big_map.update p.tid p.shop s.license_shop}

type entrypoints = 
| Buy of purchase_params
| Modify_shop of modify_shop_params


let main (e, s : entrypoints * storage) = 
    match e with 
    | Buy p -> buy (s,p)
    | Modify_shop p -> modify_shop (s, p)