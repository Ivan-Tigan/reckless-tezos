#if ! FA2_INTERFACE
#define FA2_INTERFACE
type fa12_transfer = [@layout:comb] { from : address; [@annot:to]to_: address; value: nat; }
type token_id = nat
type transfer_destination = [@layout:comb] { to_ : address; token_id : token_id; amount : nat; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_destination list; }
type balance_of_request = [@layout:comb] { owner : address; token_id : token_id; }
type balance_of_response = [@layout:comb] { request : balance_of_request; balance : nat; }
type balance_of_param = [@layout:comb] { requests : balance_of_request list; callback : (balance_of_response list) contract; }
type operator_param = [@layout:comb] { owner : address; operator : address; token_id: token_id; }
type update_operator = [@layout:comb] | Add_operator of operator_param | Remove_operator of operator_param
type token_metadata = [@layout:comb] { token_id : token_id; token_info : (string, bytes) map; }

type token_metadata_storage = (token_id, token_metadata) big_map
type token_metadata_param = [@layout:comb] { token_ids : token_id list; handler : (token_metadata list) -> unit; }
type contract_metadata = (string, bytes) big_map

(* FA2 hooks interface *)

type transfer_destination_descriptor = [@layout:comb] { to_ : address option; token_id : token_id; amount : nat; }

type transfer_descriptor = [@layout:comb] { from_ : address option; txs : transfer_destination_descriptor list }

type transfer_descriptor_param = [@layout:comb] { batch : transfer_descriptor list; operator : address; }

(*
Entrypoints for sender/receiver hooks

type fa2_token_receiver =
  ...
  | Tokens_received of transfer_descriptor_param

type fa2_token_sender =
  ...
  | Tokens_sent of transfer_descriptor_param
*)

#endif

type migration_status = Waiting of address | Working | Emigrated of address
type token_type = Fa12 of address | Fa2 of address * token_id | Tez of bool

type ledger = ((address * nat), nat) big_map
type minters = (address, token_id set) big_map
type propose_new_token_params = {token_id: token_id; token_info:bytes; initial_supply:(address * nat) list}
type approve_reject_new_token_params = {proposer: address; decision: bool} 
type token_proposals = (address, propose_new_token_params list) big_map 

type storage = {
  minters: minters;
  registrar: address;
  ledger: ledger;
  token_metadata: token_metadata_storage;
  token_proposals: token_proposals;
  metadata: contract_metadata;
  migration_status:migration_status;
  // item_mods: (token_id, bytes) big_map
//  user_metadata: (address, bytes) big_map;
}
let mk_fa12_transfer_op (a : address) (fa12tr: fa12_transfer) = match (Tezos.get_entrypoint_opt "%transfer" a : fa12_transfer contract option) with Some c -> Tezos.transaction fa12tr 0tez c | _ -> (failwith "Invalid fa1.2 contract" : operation)
// let mk_fa12_transfer_ops (fa12trs:(address * fa12_transfer) list) = List.map mk_fa12_transfer_op fa12trs
let mk_fa2_transfer_op (a:address) (fa2tr: transfer list) = match (Tezos.get_entrypoint_opt "%transfer" a : (transfer list) contract option) with Some c -> Tezos.transaction fa2tr 0tez c | _ -> (failwith "Invalid fa2 contract" : operation)
// let mk_fa2_transfer_ops (fa2trs:(address * transfer list) list) = List.map mk_fa2_transfer_op fa2trs
// let mk_transfer_ops ((f12, f2):(address * fa12_transfer) list * (address * transfer list) list) = List.fold_left (fun (acc, f2) -> (mk_fa2_transfer_op f2)::acc) (List.map mk_fa12_transfer_op f12) f2
let can_transfer (ts:transfer list) = Tezos.sender = Tezos.self_address || List.fold_left (fun ((b, t): bool * transfer) -> b && (Tezos.source = t.from_ || Tezos.sender = t.from_)) true ts
let transfer ((s, ts):storage * transfer list) = 
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
let mint ((s, tds) : storage * transfer_destination list) =
  let new_ledger = 
    List.fold_left (fun ((acc, td):ledger * transfer_destination) ->
      let _ = match (Big_map.find_opt Tezos.sender s.minters : token_id set option) with | Some ts -> (if Set.mem td.token_id ts then () else failwith "No permission to mint this token") | _ -> failwith "No minting permission" in
      let _ = if Big_map.mem td.token_id s.token_metadata then () else failwith "This token is not registered so it cannot be minted." in
      Big_map.update 
        (td.to_, td.token_id) 
        (Some (
            match (Big_map.find_opt (td.to_, td.token_id) acc : nat option) with 
            | Some n ->  n + td.amount 
            | _ -> td.amount)
        ) acc) 
        s.ledger
        tds  in
  ([]:operation list), {s with ledger = new_ledger}

let approve_reject_new_tokens ((s, rs):storage * approve_reject_new_token_params) = 
  let _ = if Tezos.sender = s.registrar then () else failwith "You have no premission to approve/reject proposals." in
  let proposal = match Big_map.find_opt rs.proposer s.token_proposals with Some ts -> ts | _ -> (failwith "Proposal does not exist.": propose_new_token_params list) in  
  let new_proposals = Big_map.remove rs.proposer s.token_proposals in
  let new_metadatas_ledger = 
    List.fold_left (fun (((acc2, acc3), r):(token_metadata_storage * ledger) * propose_new_token_params) -> 
      (if rs.decision && Big_map.mem r.token_id acc2 then (failwith "Token already registered":token_metadata_storage) else (if rs.decision then Big_map.add r.token_id ({token_id=r.token_id; token_info=Map.literal[("",r.token_info)]}) acc2 else acc2)),
      (if rs.decision then List.fold_left (fun ((l,(a, n)):ledger * (address * nat)) -> Big_map.add (a, r.token_id) n l) acc3 r.initial_supply else acc3)
    ) (s.token_metadata, s.ledger) proposal in 
  ([]:operation list), {s with token_proposals = new_proposals; token_metadata = new_metadatas_ledger.0; ledger = new_metadatas_ledger.1}
let propose_new_tokens ((s,rs):storage * propose_new_token_params list) = 
  let _ = match Big_map.find_opt Tezos.sender s.token_proposals with Some _ -> failwith "You can make 1 proposal at a time." | _ -> () in 
  ([]:operation list), {s with token_proposals = Big_map.add Tezos.sender rs s.token_proposals}


type finalize_args = {seed:nat; offer_ids:nat list}
type fa2_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  | Mint of transfer_destination list
  | Propose_tokens of propose_new_token_params list
  | Approve_reject_token_proposal of approve_reject_new_token_params
  | Initialize of storage
  | Emigrate of address
  | Modify of storage -> storage
let main (action, s : fa2_entry_points * storage) : operation list * storage =
  let _ = 
    match action, s.migration_status with 
    | Initialize _, Waiting a -> if Tezos.sender = a then () else failwith "Invalid immigration address."
    | Emigrate _, Working -> if Tezos.sender = s.registrar then () else failwith "No permission to emigrate"
    | Initialize _, _ -> failwith "Already initialized"
    | Emigrate _, _ -> failwith "Either not initialized or already migrated"
    | _, Working -> ()
    | _, _ -> failwith "Either not initialized or migrated to new version"
    in
 match action with
 | Transfer t -> let _ = if can_transfer t then () else failwith "FA2_NOT_OWNER" in transfer (s, t)
 | Mint m -> mint (s,m) 
 | Propose_tokens x -> propose_new_tokens (s,x)
 | Approve_reject_token_proposal x -> approve_reject_new_tokens (s,x)
 | Initialize s -> ([]:operation list), s
 | Emigrate a -> (match (Tezos.get_entrypoint_opt "%initialize" a : storage contract option) with Some c -> [Tezos.transaction s 0tez c], {s with migration_status = Emigrated a} | None -> failwith "Invalid destination contract" : operation list * storage)
 | Modify f -> if Tezos.sender = s.registrar then ([]:operation list), f s else (failwith "No permission to modify contract.": operation list * storage)
 | _ -> (failwith "Not implemented":operation list * storage)


