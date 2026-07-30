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
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:1:1:4
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:6:1:8
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `ff8` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │     ‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.8` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ff8.8.d                                                                   │
 │       ‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_007.md:1:6 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.d` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotInt,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
