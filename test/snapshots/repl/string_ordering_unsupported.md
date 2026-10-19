# META
~~~ini
description=String ordering operations should fail gracefully (not supported)
type=repl
~~~
# SOURCE
~~~roc
» "apple" > "banana"
» "zoo" < "aardvark"
» "equal" >= "equal"
» "first" <= "second"
~~~
# OUTPUT
**Missing Method**
This string's type never resolved to a concrete type, so it has no `is_gt` method for `>` to call.
```roc
"apple" > "banana"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_gt : a, a -> Bool]

**Hint:** None of the built-in string types support `>`.
---
**Missing Method**
This string's type never resolved to a concrete type, so it has no `is_lt` method for `<` to call.
```roc
"zoo" < "aardvark"
```
^^^^^

Its type needs all of these:

    a where [a.is_lt : a, a -> Bool]

**Hint:** None of the built-in string types support `<`.
---
**Missing Method**
This string's type never resolved to a concrete type, so it has no `is_gte` method for `>=` to call.
```roc
"equal" >= "equal"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_gte : a, a -> Bool]

**Hint:** None of the built-in string types support `>=`.
---
**Missing Method**
This string's type never resolved to a concrete type, so it has no `is_lte` method for `<=` to call.
```roc
"first" <= "second"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_lte : a, a -> Bool]

**Hint:** None of the built-in string types support `<=`.
# PROBLEMS
NIL
