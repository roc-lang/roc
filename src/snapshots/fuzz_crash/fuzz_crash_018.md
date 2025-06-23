# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level type_anno

# TOKENS
~~~zig
Int(1:1-1:2),LowerIdent(1:3-1:4),OpColon(1:4-1:5),UpperIdent(1:5-1:6),Newline(1:1-1:1),
DotUpperIdent(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(file (1:1-2:3)
	(malformed_header (1:1-1:2) "missing_header")
	(statements
		(type_anno (1:3-2:3)
			"b"
			(mod_ty "S" ".R"))))
~~~
# FORMATTED
~~~roc
b : S..R
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~