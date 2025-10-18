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
UNDECLARED TYPE - expect_stmt.md:1:8:1:12
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
KwExpect,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-expect
	(e-tag (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-expect
		(e-runtime-error (tag "undeclared_type"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
