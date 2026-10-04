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
**Type Not Determined**
Nothing in this program determines the type of this number:
```roc
35.foo()
```
^^

Its type needs all of these:

    a where [a.foo : a -> _ret]

Without knowing which type it is, there's no way to tell which `foo` method to use.

**Hint:** Add a suffix or a type annotation saying which type it should be.
# PROBLEMS
NIL
