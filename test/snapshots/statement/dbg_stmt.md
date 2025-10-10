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
NIL
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
KwDbg(1:1-1:4),UpperIdent(1:5-1:9),NoSpaceDotUpperIdent(1:9-1:14),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(s-dbg @1.1-1.14
	(e-tag @1.5-1.14 (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-dbg @1.1-1.14
		(e-runtime-error (tag "undeclared_type"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
