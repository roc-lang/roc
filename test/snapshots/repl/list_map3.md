# META
~~~ini
description=List.map3 applies a ternary function elementwise over three lists, clamping to the shortest
type=repl
~~~
# SOURCE
~~~roc
» List.map3([1, 2, 3], [10, 20, 30], [100, 200, 300], |a, b, c| a + b + c)
» List.map3([1, 2, 3, 4, 5], [10, 20], [100, 200, 300], |a, b, c| a + b + c)
» xs = ["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
» List.map3(xs, ["a second heap-allocated string used to exercise refcounting", "yet another heap-allocated string for the refcount test"], ["a third heap-allocated string in the map3 refcount check", "one final heap-allocated string to exercise element refcounting"], |a, b, c| Str.concat(a, Str.concat(b, c)))
» xs
~~~
# OUTPUT
[111.0, 222.0, 333.0]
---
[111.0, 222.0]
---
assigned `xs`
---
["a string long enough to be heap-allocated instead of stored inlinea second heap-allocated string used to exercise refcountinga third heap-allocated string in the map3 refcount check", "another string that is long enough to require a heap allocationyet another heap-allocated string for the refcount testone final heap-allocated string to exercise element refcounting"]
---
["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
# PROBLEMS
NIL
