# META
~~~ini
description=unary_negation
type=expr
~~~
# SOURCE
~~~roc
-foo
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_negation.md:1:2:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**unary_negation.md:1:2:1:5:**
```roc
-foo
```
 ^^^


# TOKENS
~~~zig
OpUnaryMinus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-ident (raw "foo")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-minus
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
