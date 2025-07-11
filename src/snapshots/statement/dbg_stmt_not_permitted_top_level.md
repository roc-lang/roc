# META
~~~ini
description=Debug expression not permitted at the top level
type=file
~~~
# SOURCE
~~~roc
module [foo]

# not permitted
dbg "foo"

foo = ...
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:4:1:6:4
# PROBLEMS
**INVALID STATEMENT**
The statement **dbg** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**dbg_stmt_not_permitted_top_level.md:4:1:6:4:**
```roc
dbg "foo"

foo = ...
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
KwDbg(4:1-4:4),StringStart(4:5-4:6),StringPart(4:6-4:9),StringEnd(4:9-4:10),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),TripleDot(6:7-6:10),EndOfFile(6:10-6:10),
~~~
# PARSE
~~~clojure
(file @1.1-6.10
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-dbg @4.1-6.4
			(e-string @4.5-4.10
				(e-string-part @4.6-4.9 (raw "foo"))))
		(s-decl @6.1-6.10
			(p-ident @6.1-6.4 (raw "foo"))
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
		(p-assign @6.1-6.4 (ident "foo"))
		(e-not-implemented @6.7-6.10)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "*")))
	(expressions
		(expr @6.7-6.10 (type "*"))))
~~~
