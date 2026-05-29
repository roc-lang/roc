# META
~~~ini
description=Bare underscore assignment to discard a value
type=snippet
~~~
# SOURCE
~~~roc
_ = 42
~~~
# EXPECTED
INVALID STATEMENT - underscore_assignment.md:1:1:1:2
# PROBLEMS
**INVALID STATEMENT**
The statement `destructuring declaration` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**underscore_assignment.md:1:1:1:2:**
```roc
_ = 42
```
^


# TOKENS
~~~zig
Underscore,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-underscore)
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
