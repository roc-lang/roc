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
TOKENIZE: (1:9-1:9) OverClosedBrace:
module P]F
        ^PARSER: header_expected_open_square
PARSER: expected_colon_after_type_annotation
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