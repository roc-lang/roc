# META
~~~ini
description=Dict capacity operations preserve or shrink bucket capacity as requested
type=repl
~~~
# SOURCE
~~~roc
» Dict.with_capacity(10).capacity() >= 10
» Dict.with_capacity(10).insert("a", 1).clear().capacity() == Dict.with_capacity(10).capacity()
» Dict.with_capacity(10).insert("a", 1).clear().insert("b", 2).get("b")
» Dict.single("a", 1).reserve(10).capacity() >= 11
» Dict.with_capacity(10).insert("a", 1).release_excess_capacity().capacity() < Dict.with_capacity(10).capacity()
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).insert("d", 4).insert("e", 5).insert("f", 6).insert("g", 7).get("g")
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).insert("d", 4).insert("e", 5).insert("f", 6).insert("g", 7).len()
~~~
# OUTPUT
True
---
True
---
Ok(2.0)
---
True
---
True
---
Ok(7.0)
---
7
# PROBLEMS
NIL
