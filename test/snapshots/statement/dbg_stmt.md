# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
dbg Bool.True
~~~
# EXPECTED
UNDECLARED TYPE - dbg_stmt.md:1:5:1:9
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**dbg_stmt.md:1:5:1:9:**
```roc
dbg Bool.True
```
    ^^^^


# TOKENS
~~~zig
KwDbg,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-dbg
	(e-tag (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-dbg
		(e-runtime-error (tag "undeclared_type"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
