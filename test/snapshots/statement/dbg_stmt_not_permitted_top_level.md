# META
~~~ini
description=Debug expression not permitted at the top level
type=snippet
~~~
# SOURCE
~~~roc
# not permitted
dbg "foo"

foo = ...
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:2:1:2:10
# PROBLEMS
**INVALID STATEMENT**
The statement `dbg` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**dbg_stmt_not_permitted_top_level.md:2:1:2:10:**
```roc
dbg "foo"
```
^^^^^^^^^


# TOKENS
~~~zig
KwDbg,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-dbg
			(e-string
				(e-string-part (raw "foo"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-not-implemented)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
