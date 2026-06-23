# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
PARSE ERROR - fuzz_crash_015.md:1:1:1:4
PARSE ERROR - fuzz_crash_015.md:1:4:1:6
PARSE ERROR - fuzz_crash_015.md:2:1:2:4
PARSE ERROR - fuzz_crash_015.md:3:1:3:4
PARSE ERROR - fuzz_crash_015.md:3:4:3:6
PARSE ERROR - fuzz_crash_015.md:4:1:4:3
# PROBLEMS

LEADING ZERO

Numbers cannot have leading zeros.



┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0o0.0                                                                     │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0o0.0                                                                     │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0_0                                                                       │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:2:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0u8.0                                                                     │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:3:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0u8.0                                                                     │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:3:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0_                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:4:1 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
Int,NoSpaceDotInt,
Int,
Int,NoSpaceDotInt,
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
