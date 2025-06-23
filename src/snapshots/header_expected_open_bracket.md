# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred.

# TOKENS
~~~zig
KwModule(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(file (1:1-1:7)
	(malformed_header (1:7-1:7) "header_expected_open_square")
	(statements))
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