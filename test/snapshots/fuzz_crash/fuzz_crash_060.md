# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
0"
}
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_060.md:1:2:1:3
UNEXPECTED STATEMENT - fuzz_crash_060.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_060.md:1:2:1:3
UNEXPECTED STATEMENT - fuzz_crash_060.md:1:3:1:3
UNEXPECTED STATEMENT - fuzz_crash_060.md:1:3:1:3
UNEXPECTED STATEMENT - fuzz_crash_060.md:2:1:2:2
# PROBLEMS

┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  0"                                                                        │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:1:2 ┘



┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0"                                                                        │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0"                                                                        │
 │   ‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:1:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0"                                                                        │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:1:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I reached the end of the file before this construct was complete.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0"                                                                        │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:1:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I reached the end of the file before this construct was complete.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_060.md:2:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
Int,StringStart,StringPart,StringEnd,
CloseCurly,
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
