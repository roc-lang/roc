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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
KwDbg(4:1-4:4),StringStart(4:5-4:6),StringPart(4:6-4:9),StringEnd(4:9-4:10),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),TripleDot(6:7-6:10),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.10
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
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
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "_a")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @1.1-1.1 (type "_a"))))
~~~
