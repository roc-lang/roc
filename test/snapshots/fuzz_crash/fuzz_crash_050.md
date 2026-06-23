# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
)
 
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_050.md:1:1:1:2
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  )                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_050.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
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
