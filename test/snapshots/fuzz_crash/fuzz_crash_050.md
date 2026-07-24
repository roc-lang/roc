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
UNEXPECTED STATEMENT - fuzz_crash_050.md:1:1:1:2
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  )                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_050.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
