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
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:2:1:3
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:3:1:4
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:4:1:5
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:5:1:6
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:6:1:16
UNEXPECTED STATEMENT - fuzz_crash_012.md:1:16:1:17
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:5 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │       ‾‾‾‾‾‾‾‾‾‾                                                           │
 └───────────────────────────────────────────────────── fuzz_crash_012.md:1:6 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `l888888888` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ||(|(l888888888|                                                          │
 │                 ‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_012.md:1:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.

# TOKENS
~~~zig
OpBar,OpBar,NoSpaceOpenRound,OpBar,NoSpaceOpenRound,LowerIdent,OpBar,
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
