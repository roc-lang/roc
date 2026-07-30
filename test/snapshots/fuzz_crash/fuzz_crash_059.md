# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:3:2:5
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:6:2:7
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:7:2:8
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:8:2:9
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:9:2:13
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:13:2:14
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:14:2:15
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:15:2:16
MOD NOT FOUND - fuzz_crash_059.md:1:20:2:2
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │    ‾‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_059.md:2:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `if` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │       ‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_059.md:2:6 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_059.md:2:7 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `{` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_059.md:2:8 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │          ‾‾‾‾                                                              │
 └───────────────────────────────────────────────────── fuzz_crash_059.md:2:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `else` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │              ‾                                                             │
 └──────────────────────────────────────────────────── fuzz_crash_059.md:2:13 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │               ‾                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_059.md:2:14 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `|` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  G if 0{}else||0                                                           │
 │                ‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_059.md:2:15 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────┐
│ MOD NOT FOUND ├─ The mod `B` was not found in this Roc project. ──────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  app[]{f:platform""}import B as                                            │
 │  G if 0{}else||0                                                           │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_059.md:1:20 ┘


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,UpperIdent,KwAs,
UpperIdent,KwIf,Int,OpenCurly,CloseCurly,KwElse,OpBar,OpBar,Int,
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
		(s-import (raw "B") (alias "G"))
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
import B as G
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "B")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
