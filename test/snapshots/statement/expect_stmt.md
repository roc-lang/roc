# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
expect Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt.md:1:8:1:12:**
```roc
expect Bool.True
```
       ^^^^


# TOKENS
~~~zig
KwExpect(1:1-1:7),UpperIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(s-expect @1.1-1.17
	(e-tag @1.8-1.17 (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-expect @1.1-1.17
		(e-runtime-error (tag "undeclared_type"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
