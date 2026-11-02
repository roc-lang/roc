# META
~~~ini
description=Method call directly on integer literal
type=repl
~~~
# SOURCE
~~~roc
» 35.foo()
~~~
# OUTPUT
**Missing Method**
This number's type never resolved to a concrete type, so it has no `foo` method.
```roc
35.foo()
```
^^

Its type needs all of these:

    a where [a.foo : a -> _ret]

**Hint:** None of the built-in number types have a method named `foo`.
# PROBLEMS
NIL
