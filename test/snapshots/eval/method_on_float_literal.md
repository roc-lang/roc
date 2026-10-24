# META
~~~ini
description=Method call directly on float literal
type=repl
~~~
# SOURCE
~~~roc
» 12.34.foo()
~~~
# OUTPUT
**Missing Method**
This number's type never resolved to a concrete type, so it has no `foo` method.
```roc
12.34.foo()
```
^^^^^

Its type needs all of these:

    a where [a.foo : a -> _ret]

**Hint:** None of the built-in number types have a method named `foo`.
# PROBLEMS
NIL
