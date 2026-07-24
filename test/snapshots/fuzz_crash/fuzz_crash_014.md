# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_014.md:1:1:1:3
UNEXPECTED STATEMENT - fuzz_crash_014.md:1:3:1:5
UNEXPECTED STATEMENT - fuzz_crash_014.md:2:1:2:6
UNEXPECTED STATEMENT - fuzz_crash_014.md:3:1:3:5
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0b.0                                                                      │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_014.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0b` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0b.0                                                                      │
 │    ‾‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_014.md:1:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0bu22                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_014.md:2:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0bu22` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0u22                                                                      │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_014.md:3:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0u22` here.

# TOKENS
~~~zig
MalformedNumberNoDigits,NoSpaceDotInt,
MalformedNumberNoDigits,
MalformedNumberBadSuffix,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
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
