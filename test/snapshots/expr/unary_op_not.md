# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_op_not.md:1:2:1:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `isValid` in this scope.
Is there an `import` or `exposing` missing up-top?

**unary_op_not.md:1:2:1:9:**
```roc
!isValid
```
 ^^^^^^^


# TOKENS
~~~zig
OpBang(1:1-1:2),LowerIdent(1:2-1:9),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(unary "!"
	(e-ident @1.2-1.9 (raw "isValid")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not @1.1-1.9
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "Error"))
~~~
