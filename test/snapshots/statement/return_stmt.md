# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
return Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
**RETURN OUTSIDE FUNCTION**
The `return` keyword can only be used inside function bodies.

**return_stmt.md:1:1:1:17:**
```roc
return Bool.True
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwReturn,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-return
	(e-tag (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-runtime-error (tag "return_outside_fn")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
