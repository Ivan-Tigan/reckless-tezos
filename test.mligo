#include "fa2_reckless.mligo"
#include "init_fa2_reckless,mligo"
let test = 
    let one_day: int = 86_400 in
    // let () = Test.bootstrap_contract 1mutez main { initial_storage with ledger = (Big_map.literal [(("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ":address),0n), 200n; (("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ":address),1000n), 1n] : ((address * nat), nat) big_map) } in
    let () = Test.reset_state 10n ([]:tez list) in
    let () = Test.set_now ("2021-10-01t10:10:10Z" : timestamp) in
    let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
    let (taddr, _,_) = Test.originate main { initial_storage with ledger = (Big_map.literal [(a1,0n), 200n; (a1,1000n), 1n] : ((address * nat), nat) big_map) } 0tez in
    // let taddr = (Test.nth_bootstrap_typed_address 0n : (fa2_entry_points, storage) typed_address) in
    // let addr = Test.nth_bootstrap_contract 1n in
    let contr = Test.to_contract taddr in
    let () = Test.set_source a1 in
    // let () = Test.log (taddr, addr) in
    let () = Test.transfer_to_contract_exn contr (Transfer [{from_ = a1; txs=[{to_ = a2; token_id = 0n; amount = 10n}] }]) 1mutez in 
    let ns = Test.get_storage taddr in
    let () = Test.log ("a1", a1) in
    let () = Test.log ("a2", a2) in
    let () = Test.log ns.ledger in
    let () = Test.log (a1, Big_map.find_opt (a1,0n) ns.ledger) in
    // let () = Test.log (Test.last_originations()) in
    let () = Test.transfer_to_contract_exn contr (Post [ { seller = a1; price = a1, Fa2(Tezos.address contr, 0n), 10n; product = [[1000n,1n,100n]]; start_ = ("2021-10-02t10:10:10Z" : timestamp); end_ = ("2021-10-02t10:15:10Z" : timestamp) + one_day; }]) 1mutez in 
    let ns = Test.get_storage taddr in
    let () = Test.log ns.ah.offers in
    assert (Big_map.find_opt (a1,0n) ns.ledger = Some 190n)