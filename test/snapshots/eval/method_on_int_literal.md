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
This `foo` method is being called on a value whose type doesn't have that method.
```roc
35.foo()
```
   ^^^

The value's type, which does not have a method named `foo`, is:

    Dec

**Hint:** This numeric literal was given the type `Dec` because it was never used as any concrete number type. To use a different numeric type, add a suffix or a type annotation.
# PROBLEMS
NIL
