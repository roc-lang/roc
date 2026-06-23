# META
~~~ini
description=Expected an open bracket for the header
type=file
~~~
# SOURCE
~~~roc
module
~~~
# EXPECTED
PARSE ERROR - header_expected_open_bracket.md:2:1:2:1
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: header_expected_open_square ───────┐
└┬────────────┘                                                               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └─────────────────────────────────────── header_expected_open_bracket.md:2:1 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
KwModule,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
	(statements))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
