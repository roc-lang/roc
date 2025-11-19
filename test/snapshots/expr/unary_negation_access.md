# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_negation_access.md:1:2:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `rec1` in this scope.
Is there an `import` or `exposing` missing up-top?

**unary_negation_access.md:1:2:1:6:**
```roc
-rec1.field
```
 ^^^^


# TOKENS
~~~zig
OpUnaryMinus,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-field-access
		(e-ident (raw "rec1"))
		(e-ident (raw "field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-minus
	(e-dot-access (field "field")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
