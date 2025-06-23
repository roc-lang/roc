# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# PROBLEMS
**OVER CLOSED BRACE**
There are too many closing braces here.

**PARSE ERROR**
A parsing error occurred.

**PARSE ERROR**
A parsing error occurred.

# TOKENS
~~~zig
KwModule(1:1-1:7),UpperIdent(1:8-1:9),UpperIdent(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(file (1:1-1:11)
	(malformed_header (1:8-1:9) "header_expected_open_square")
	(statements (malformed_stmt (1:10-1:11) "expected_colon_after_type_annotation")))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~