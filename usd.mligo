#include "fa2_reckless.mligo"
type usd_storage = int
type fa12_entrypoint = 
| [@annot:transfer] Transfer2 of fa12_transfer
| Nonsense of string

let usd_main (a,s : fa12_entrypoint * usd_storage) = 
    (([]:operation list),s)