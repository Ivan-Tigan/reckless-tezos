#include "fa2_reckless.mligo"
#include "init_fa2_reckless,mligo"
let t () = 
    let one_day: int = 86_400 in
    let () = Test.reset_state 10n ([]:tez list) in
    let () = Test.set_now ("2021-10-02t10:10:10Z" : timestamp) in
    let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
    let (taddr, _,_) = Test.originate main { initial_storage with registrar = a2; ledger = (Big_map.literal [(a1,0n), 200n; (a1,1000n), 1n; (a1, 1001n), 1n] : ((address * nat), nat) big_map) } 0tez in
    let contr = Test.to_contract taddr in
    let () = Test.set_source a1 in
    let () = Test.transfer_to_contract_exn contr (Transfer [{from_ = a1; txs=[{to_ = a2; token_id = 0n; amount = 50n}] }]) 1mutez in 
    let ns = Test.get_storage taddr in
    // let () = assert (Big_map.find_opt (a1,0n) ns.ledger = Some 150n) in
    let () = Test.log ("a1", a1) in
    let () = Test.log ("a2", a2) in
    let () = Test.log ns.ledger in
    let () = Test.log (a1, Big_map.find_opt (a1,0n) ns.ledger) in
    let () = Test.transfer_to_contract_exn contr (Post [ { seller = a1; price = a1, Fa2(Tezos.address contr, 0n), 10n; product = [[1000n,1n,50n; 1001n,1n,50n]]; start_ = ("2021-10-02t10:10:15Z" : timestamp); end_ = ("2021-10-02t10:10:20Z" : timestamp) + one_day; }]) 1mutez in 
    let ns = Test.get_storage taddr in
    let () = Test.log ns.ah.offers in
    let () = Test.set_source a2 in
    let () = Test.transfer_to_contract_exn contr (Bid [0n,15n]) 1mutez in 
    let ns = Test.get_storage taddr in let () = Test.log ns.ah.offers in
    let () = Test.set_now ("2021-10-03t10:10:25Z" : timestamp) in
    let () = Test.transfer_to_contract_exn contr (Finalize {seed=60n; offer_ids = [0n]}) 1mutez in 
    let ns = Test.get_storage taddr in let () = Test.log ns.ah in let () = Test.log ns.ledger in

    let (taddr2, _,_) = Test.originate main { initial_storage with migration_status = Waiting (Tezos.address contr) } 1tez in
    let contr2 = Test.to_contract taddr2 in
    let () = Test.transfer_to_contract_exn contr (Emigrate (Tezos.address contr2)) 5mutez in 
    let ns2 = Test.get_storage taddr2 in
    let () = Test.log (ns.ledger, ns2.ledger) in
    ()

let test_migration()  = 

    let () = Test.reset_state 10n ([]:tez list) in
    let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
    let (taddr, _,_) = Test.originate main { initial_storage with registrar = a2; ledger = (Big_map.literal [(a1,0n), 200n; (a1,1000n), 1n; (a1, 1001n), 1n] : ((address * nat), nat) big_map) } 0tez in
    let contr = Test.to_contract taddr in
    let () = Test.set_source a1 in
    let () = Test.transfer_to_contract_exn contr (Transfer [{from_ = a1; txs=[{to_ = a2; token_id = 0n; amount = 50n}] }]) 1mutez in 
    let ns = Test.get_storage taddr in let () = Test.log ns.ah in let () = Test.log ns.ledger in
    // let (taddr2, _,_) = Test.originate main { initial_storage with migration_status = Waiting (Tezos.address contr) } 1tez in
    let (taddr2, _,_) = Test.originate main { initial_storage with migration_status = Waiting(Tezos.address contr) } 1tez in
    let contr2 = Test.to_contract taddr2 in
    let () = Test.set_source a2 in
    let () = Test.transfer_to_contract_exn contr (Emigrate (Tezos.address contr2)) 5mutez in 
    // let ns2 = Test.get_storage taddr2 in
    // let () = Test.log (ns.ledger, ns2.ledger) in
    ()

#include "usd.mligo"
#include "mystery_locker.mligo"
let test_mystery_tokens = 
    let () = Test.reset_state 10n ([]:tez list) in
    let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
    let (usd_taddr,_,_) = Test.originate usd_main 0 0tez in 
    let usd_contr = Test.to_contract usd_taddr in
    let usd_address = Tezos.address usd_contr in
    let (rex_taddr, _,_) = Test.originate main { initial_storage with registrar = a2; ledger = (Big_map.literal [(a1,0n), 200n; (a1,1000n), 1n; (a1, 1001n), 1n] : ((address * nat), nat) big_map) ; } 0tez in
    let rex_contr = Test.to_contract rex_taddr in
    let rex_addr = Tezos.address rex_contr in
    let (myst_taddr, _,_) = Test.originate MysteryTokenLocker.main MysteryTokenLocker.empty_storage 0tez in
    let myst_contr = Test.to_contract myst_taddr in
    let () = Test.set_source a1 in
    let () = Test.transfer_to_contract_exn myst_contr (Lock (5n, {contents = Map.literal [0n, (Fa2(rex_addr, 1000n),1n)]; total_supply = 1n; remaining_supply = 1n; currency = Fa12(usd_address); base_price = 1n; max_price = 2n; owner=a1})) 0tez in
    let () = Test.transfer_to_contract_exn myst_contr (Buy [5n,1n]) 0tez in
    let rex_s = Test.get_storage rex_taddr in
    let () = Test.log myst_taddr in 
    let () = Test.log rex_s.ledger in 
    let () = Test.transfer_to_contract_exn myst_contr (Redeem (542n,5n,1n)) 0tez in
    let myst_s = Test.get_storage myst_taddr in
    let () = Test.log myst_s in 
    ()
    


#include "dutest.mligo"
let test_du () = 

    let () = Test.reset_state 10n ([]:tez list) in
    let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
    let (taddr1, _,_) = Test.originate main 5n 0tez in
    let (taddr2, _,_) = Test.originate main 3n 0tez in
    let contr1 = Test.to_contract taddr1 in
    let contr2 = Test.to_contract taddr2 in
    let () = Test.set_source a2 in
    let () = Test.transfer_to_contract_exn contr1 (Emig (Tezos.address contr2)) 5mutez in 
    let ns2 = Test.get_storage taddr2 in
    let () = Test.log ns2 in
    ()
