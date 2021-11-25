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

type ledger = ((address * nat), nat) big_map

let can_transfer (ts:transfer list) = Tezos.sender = Tezos.self_address || List.fold_left (fun ((b, t): bool * transfer) -> b && (Tezos.source = t.from_ || Tezos.sender = t.from_)) true ts
let transfer ((ledger, ts):ledger * transfer list) = 
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
      ) l t.txs) ledger ts in
  ([]:operation list), new_ledger

let mk_fa12_transfer_op (a : address) (fa12tr: fa12_transfer) = match (Tezos.get_entrypoint_opt "%transfer" a : fa12_transfer contract option) with Some c -> Tezos.transaction fa12tr 0tez c | _ -> (failwith "Invalid fa1.2 contract" : operation)
// let mk_fa12_transfer_ops (fa12trs:(address * fa12_transfer) list) = List.map mk_fa12_transfer_op fa12trs
let mk_fa2_transfer_op (a:address) (fa2tr: transfer list) = match (Tezos.get_entrypoint_opt "%transfer" a : (transfer list) contract option) with Some c -> Tezos.transaction fa2tr 0tez c | _ -> (failwith "Invalid fa2 contract" : operation)
// let mk_fa2_transfer_ops (fa2trs:(address * transfer list) list) = List.map mk_fa2_transfer_op fa2trs
// let mk_transfer_ops ((f12, f2):(address * fa12_transfer) list * (address * transfer list) list) = List.fold_left (fun (acc, f2) -> (mk_fa2_transfer_op f2)::acc) (List.map mk_fa12_transfer_op f12) f2