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

    Dec

**Hint:** This numeric literal was given the type **Dec** because it was never used as any concrete number type. To use a different numeric type, add a suffix or a type annotation.

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
