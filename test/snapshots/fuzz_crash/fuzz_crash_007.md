# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_007.md:1:1:1:4
PARSE ERROR - fuzz_crash_007.md:1:4:1:6
PARSE ERROR - fuzz_crash_007.md:1:6:1:8
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │       ‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:6 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotInt,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
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
