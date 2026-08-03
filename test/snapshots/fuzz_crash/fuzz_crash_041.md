# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:20:1:21
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:21:1:22
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:22:1:23
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:23:1:24
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:24:1:25
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:25:1:26
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:26:1:27
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:27:1:28
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:28:1:29
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                     ‾                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:20 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                      ‾                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:21 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                       ‾                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:22 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                        ‾                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:23 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                         ‾                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:24 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                          ‾                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:25 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                           ‾                                                │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:26 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                            ‾                                               │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:27 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}|(0,)|||0                                              │
 │                             ‾                                              │
 └──────────────────────────────────────────────────── fuzz_crash_041.md:1:28 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.

# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpBar,NoSpaceOpenRound,Int,Comma,CloseRound,OpBar,OpBar,OpBar,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
app [] { f: platform "" }
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
