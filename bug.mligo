
let concat (xs: _a list) (ys: _a list) : _a list = List.fold_right (fun ((x, acc): _a * _a list) -> x::acc) xs ys 
let bindi (fm: nat -> _a -> _b list) (xs: _a list) : _b list = let (_, r) = List.fold_left (fun (((i, acc), x): (nat * _b list) * _a) -> i+1n, concat acc (fm i x) ) (0n, ([]: _b list)) xs in r
let try_remove (i:nat) (xs: _a list) : _a option * _a list = 
    List.head_opt xs, bindi (fun (j:nat) (x:_a) -> if i = j then [] else [x] ) xs
let test = 
    // let () = Test.log ("foldri", fold_right_i (fun (i:nat) ((x,acc) : nat * nat) -> i+x+acc) [4n;5n;6n] 0n) in
    let () = Test.log ("try remove", try_remove 2n [4;5;6]) in
    ()
