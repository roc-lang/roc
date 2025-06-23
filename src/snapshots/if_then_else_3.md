# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else 2
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize if_then_else expression

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
Int(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),Int(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(if_then_else (1:1-3:9)
	(ident (1:4-1:8) "" "bool")
	(block (1:9-3:2)
		(statements (int (2:2-2:3) "1")))
	(int (3:8-3:9) "2"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:1) "not_implemented")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~