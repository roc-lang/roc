# META
~~~ini
description=Bang operator on numeric literal should produce type error
type=expr
~~~
# SOURCE
~~~roc
!3
~~~
# EXPECTED
MISSING METHOD - bang_on_numeric_literal.md:1:1:1:3
# PROBLEMS
**MISSING METHOD**
This **not** method is being called on a value whose type doesn't have that method:
**bang_on_numeric_literal.md:1:1:1:3:**
```roc
!3
```
^^

The value's type, which does not have a method named **not**, is:

    a
      where [
        a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        a.not : a -> a,
      ]

**Hint:** For this to work, the type would need to have a method named **not** associated with it in the type's declaration.

# TOKENS
~~~zig
OpBang,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-int (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
