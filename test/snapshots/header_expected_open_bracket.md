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
EXPECTED EXPOSING LIST - header_expected_open_bracket.md:2:1:2:1
# PROBLEMS

┌────────────────────────┐
│ EXPECTED EXPOSING LIST ├─ I was parsing a module or hosted header, and I ───┐
└┬───────────────────────┘  expected an opening `[`.                          │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └─────────────────────────────────────── header_expected_open_bracket.md:2:1 ┘

    The names exposed by this module are written in square brackets after the
    header keyword.

    For example:
        module [main, helper]

    I reached the end of the file before this construct was complete.

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
