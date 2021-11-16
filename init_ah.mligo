let rex_address = ("KT1HjTaKovPkWC1hUAvKs3ARzsa71aGzHoKM":address) in
let admin_address = ("tz1Q3v9gjkG8gBHVDtG1aihop3dUVpEkvyUG":address) in
{
  admin = admin_address;
  fa2 = rex_address;
  fees = Big_map.literal [rex_address, (99n, Map.literal [admin_address, 1n])]; 
  ah = {
    counter = 0n;
    price_ids = (Set.literal [] : token_type set);
    offers = (Big_map.empty : (nat, offer) big_map); 
  };
  metadata= (Big_map.literal [("",0x68747470733a2f2f676973742e67697468756275736572636f6e74656e742e636f6d2f4976616e2d546967616e2f63313037653831373430386330616233323237636532376331653039303764652f7261772f623230616536393635333235663766366461323266306363353030373839316164393865633966382f5265636b6c657373253235323041756374696f6e2532353230486f757365)]: contract_metadata);
  migration_status = Working;
}
