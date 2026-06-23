# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0|
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_016.md:1:1:1:2
PARSE ERROR - fuzz_crash_016.md:1:2:1:3
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0|                                                                        │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_016.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0|                                                                        │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_016.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
Int,OpBar,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
