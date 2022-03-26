empty : Dict k v
single : k, v -> Dict k v
get : Dict k v, k -> Result v [ KeyNotFound ]*
walk : Dict k v, state, (state, k, v -> state) -> state
insert : Dict k v, k, v -> Dict k v
len : Dict k v -> Nat
remove : Dict k v, k -> Dict k v
contains : Dict k v, k -> Bool
keys : Dict k v -> List k
values : Dict k v -> List v
union : Dict k v, Dict k v -> Dict k v
intersection : Dict k v, Dict k v -> Dict k v
difference : Dict k v, Dict k v -> Dict k v
