# META
~~~ini
description=Debug expression not permitted at the top level
type=file:DbgStmtNotPermittedTopLevel.roc
~~~
# SOURCE
~~~roc
DbgStmtNotPermittedTopLevel := {}

# not permitted
dbg "foo"

foo = ...
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:4:1:4:10
# PROBLEMS
**INVALID STATEMENT**
The statement `dbg` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**dbg_stmt_not_permitted_top_level.md:4:1:4:10:**
```roc
dbg "foo"
```
^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:28),OpColonEqual(1:29-1:31),OpenCurly(1:32-1:33),CloseCurly(1:33-1:34),
KwDbg(4:1-4:4),StringStart(4:5-4:6),StringPart(4:6-4:9),StringEnd(4:9-4:10),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),TripleDot(6:7-6:10),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.10
	(type-module @1.1-1.28)
	(statements
		(s-type-decl @1.1-1.34
			(header @1.1-1.28 (name "DbgStmtNotPermittedTopLevel")
				(args))
			(ty-record @1.32-1.34))
		(s-dbg @4.1-4.10
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
		(e-not-implemented @1.1-1.1))
	(s-nominal-decl @1.1-1.34
		(ty-header @1.1-1.28 (name "DbgStmtNotPermittedTopLevel"))
		(ty-record @1.32-1.34)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "_a")))
	(type_decls
		(nominal @1.1-1.34 (type "DbgStmtNotPermittedTopLevel")
			(ty-header @1.1-1.28 (name "DbgStmtNotPermittedTopLevel"))))
	(expressions
		(expr @1.1-1.1 (type "_a"))))
~~~
