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
UNEXPECTED STATEMENT - fuzz_crash_015.md:1:1:1:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_015.md:2:1:2:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:3:1:3:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:3:4:3:6
UNEXPECTED STATEMENT - fuzz_crash_015.md:4:1:4:3
# PROBLEMS

LEADING ZERO

Numbers cannot have leading zeros.



┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0o0.0                                                                     │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0o0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0o0.0                                                                     │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:1:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0_0                                                                       │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:2:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0_0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0u8.0                                                                     │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:3:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0u8` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0u8.0                                                                     │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:3:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0_                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_015.md:4:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0_` here.

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
	(type-mod)
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
