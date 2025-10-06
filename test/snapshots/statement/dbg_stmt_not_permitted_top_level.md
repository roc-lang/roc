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
KwDbg(2:1-2:4),StringStart(2:5-2:6),StringPart(2:6-2:9),StringEnd(2:9-2:10),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),TripleDot(4:7-4:10),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @2.1-4.10
	(type-module @2.1-2.4)
	(statements
		(s-dbg @2.1-2.10
			(e-string @2.5-2.10
				(e-string-part @2.6-2.9 (raw "foo"))))
		(s-decl @4.1-4.10
			(p-ident @4.1-4.4 (raw "foo"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
# not permitted
# not permitted
dbg "foo"

foo = ...
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-not-implemented @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "_a")))
	(expressions
		(expr @1.1-1.1 (type "_a"))))
~~~
