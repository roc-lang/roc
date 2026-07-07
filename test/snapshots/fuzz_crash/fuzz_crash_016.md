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
UNEXPECTED STATEMENT - fuzz_crash_016.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_016.md:1:2:1:3
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0|                                                                        │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_016.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0|                                                                        │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_016.md:1:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.

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
