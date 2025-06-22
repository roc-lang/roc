# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# PROBLEMS
~~~txt
PARSER: missing_header
PARSER: pattern_unexpected_token
PARSER: pattern_unexpected_token
PARSER: expected_expr_bar
INVALID STATEMENT
The statement expr is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.
~~~
# TOKENS
~~~zig
OpBar(1:2-1:3),OpBar(1:3-1:4),NoSpaceOpenRound(1:4-1:5),OpBar(1:5-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:17),OpBar(1:17-1:18),EndOfFile(1:18-1:18),
~~~
# PARSE
~~~clojure
(file (1:2-1:18)
	(malformed_header (1:2-1:3) "missing_header")
	(statements (malformed_expr (1:18-1:18) "expected_expr_bar")))
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