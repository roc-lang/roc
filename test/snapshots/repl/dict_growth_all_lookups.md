# META
~~~ini
description=Dict growth and rehashing preserve every existing lookup
type=repl
~~~
# SOURCE
~~~roc
» keys = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
» dict = List.fold(keys, Dict.empty(), |acc, key| acc.insert(key, key * 10))
» dict.len()
» List.fold(keys, True, |all_found, key| all_found and dict.get(key) == Ok(key * 10))
» [dict.get(0), dict.get(6), dict.get(12), dict.get(18), dict.get(23), dict.get(24)]
~~~
# OUTPUT
assigned `keys`
---
assigned `dict`
---
24
---
True
---
[Ok(0.0), Ok(60.0), Ok(120.0), Ok(180.0), Ok(230.0), Err(KeyNotFound)]
# PROBLEMS
NIL
