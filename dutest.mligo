type storage = nat
type entrypoint = Add of nat | Init of storage | Emig of address
let main (action, s : entrypoint * storage) : operation list * storage = 
    match action with 
    | Add n -> ([]:operation list), s + n
    | Init s2 -> ([]:operation list), s2
    | Emig a -> (match (Tezos.get_entrypoint_opt "%init" a : storage contract option) with Some c -> [Tezos.transaction s 0tez c], s | None -> failwith "Invalid destination contract" : operation list * storage)
