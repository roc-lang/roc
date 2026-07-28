# META
~~~ini
description=List.map4 applies a quaternary function elementwise over four lists, clamping to the shortest
type=repl
~~~
# SOURCE
~~~roc
» List.map4([1, 2, 3], [10, 20, 30], [100, 200, 300], [1000, 2000, 3000], |a, b, c, d| a + b + c + d)
» List.map4([1, 2, 3, 4, 5], [10, 20], [100, 200, 300], [1000, 2000, 3000, 4000], |a, b, c, d| a + b + c + d)
» xs = ["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
» List.map4(xs, ["a second heap-allocated string used to exercise refcounting", "yet another heap-allocated string for the refcount test"], ["a third heap-allocated string in the map4 refcount check", "a fourth heap-allocated string in the map4 refcount check"], ["one final heap-allocated string to exercise element refcounting", "and one more heap-allocated string for the map4 refcount test"], |a, b, c, d| Str.concat(a, Str.concat(b, Str.concat(c, d))))
» xs
~~~
# OUTPUT
[1111.0, 2222.0, 3333.0]
---
[1111.0, 2222.0]
---
assigned `xs`
---
["a string long enough to be heap-allocated instead of stored inlinea second heap-allocated string used to exercise refcountinga third heap-allocated string in the map4 refcount checkone final heap-allocated string to exercise element refcounting", "another string that is long enough to require a heap allocationyet another heap-allocated string for the refcount testa fourth heap-allocated string in the map4 refcount checkand one more heap-allocated string for the map4 refcount test"]
---
["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"]
# PROBLEMS
NIL
