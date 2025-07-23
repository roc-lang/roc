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

**Undefined Variable**
The variable 'rec1' is not defined:
**unary_negation_access.md:1:2:1:6:**
```roc
-rec1.field
```
 ^^^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),LowerIdent(1:2-1:6),NoSpaceDotLowerIdent(1:6-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(unary "-"
	(e-field-access @1.2-1.12
		(e-ident @1.2-1.6 (raw "rec1"))
		(e-ident @1.6-1.12 (raw "field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-minus @1.1-1.12
	(e-dot-access @1.2-1.12 (field "field")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "Num(_size)"))
~~~
