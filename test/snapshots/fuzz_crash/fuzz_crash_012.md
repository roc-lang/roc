# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_012.md:1:1:1:2
PARSE ERROR - fuzz_crash_012.md:1:2:1:3
PARSE ERROR - fuzz_crash_012.md:1:3:1:4
PARSE ERROR - fuzz_crash_012.md:1:4:1:5
PARSE ERROR - fuzz_crash_012.md:1:5:1:6
PARSE ERROR - fuzz_crash_012.md:1:6:1:16
PARSE ERROR - fuzz_crash_012.md:1:16:1:17
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:5 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │       ‾‾‾‾‾‾‾‾‾‾                                                           │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:6 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │                 ‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_012.md:1:16 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
OpBar,OpBar,NoSpaceOpenRound,OpBar,NoSpaceOpenRound,LowerIdent,OpBar,
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
