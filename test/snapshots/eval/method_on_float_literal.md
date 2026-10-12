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
**Type Not Determined**
Nothing in this program determines the type of this number:
```roc
12.34.foo()
```
^^^^^

Its type needs all of these:

    a where [a.foo : a -> _ret]

Without knowing which type it is, there's no way to tell which `foo` method to use.

**Hint:** None of the built-in number types have a method named `foo`.
# PROBLEMS
NIL
