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
**Type Not Determined**
Nothing in this program determines the type of this string:
```roc
"apple" > "banana"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_gt : a, a -> Bool]

Without knowing which type it is, there's no way to tell which `>` to use.

**Hint:** None of the built-in string types support `>`.
---
**Type Not Determined**
Nothing in this program determines the type of this string:
```roc
"zoo" < "aardvark"
```
^^^^^

Its type needs all of these:

    a where [a.is_lt : a, a -> Bool]

Without knowing which type it is, there's no way to tell which `<` to use.

**Hint:** None of the built-in string types support `<`.
---
**Type Not Determined**
Nothing in this program determines the type of this string:
```roc
"equal" >= "equal"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_gte : a, a -> Bool]

Without knowing which type it is, there's no way to tell which `>=` to use.

**Hint:** None of the built-in string types support `>=`.
---
**Type Not Determined**
Nothing in this program determines the type of this string:
```roc
"first" <= "second"
```
^^^^^^^

Its type needs all of these:

    a where [a.is_lte : a, a -> Bool]

Without knowing which type it is, there's no way to tell which `<=` to use.

**Hint:** None of the built-in string types support `<=`.
# PROBLEMS
NIL
