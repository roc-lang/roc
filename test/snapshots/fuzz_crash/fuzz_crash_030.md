# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ .
		] # Cse
	packages # Cd
		{ # pen
pkg: 77"..c", mm} #
	provides # Cd
		[ # pen
ar,
		]
~~~
# EXPECTED
EXPECTED EXPOSED NAME - fuzz_crash_030.md:8:5:8:6
EXPECTED CLOSING BRACE - fuzz_crash_030.md:11:3:11:4
EXPECTED PROVIDES - fuzz_crash_030.md:12:9:12:12
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:12:12:13
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:13:12:14
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:15:12:17
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:17:12:18
UNEXPECTED STATEMENT - fuzz_crash_030.md:13:2:13:10
UNEXPECTED STATEMENT - fuzz_crash_030.md:14:3:14:4
UNEXPECTED STATEMENT - fuzz_crash_030.md:15:1:15:3
UNEXPECTED STATEMENT - fuzz_crash_030.md:15:3:15:4
UNEXPECTED STATEMENT - fuzz_crash_030.md:16:3:16:4
# PROBLEMS

┌───────────────────────┐
│ EXPECTED EXPOSED NAME ├─ I was parsing an exposing list, and I expected ────┐
└┬──────────────────────┘  an exposed name.                                   │
 │                                                                            │
 │  [ .                                                                       │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_030.md:8:5 ┘

    Exposing lists contain lowercase values, uppercase types or tags, and
    `Type.*` entries.

    For example:
        package [main, Result, Result.*]

    I found `.` here.


┌────────────────────────┐
│ EXPECTED CLOSING BRACE ├─ I was parsing a `packages` record, and I ─────────┐
└┬───────────────────────┘  expected a closing `}`.                           │
 │                                                                            │
 │  { # pen                                                                   │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:11:3 ┘

    Close the packages record after the last package entry.

    For example:
        packages { base: "../base/main.roc" }

    I found `{` here.


┌───────────────────┐
│ EXPECTED PROVIDES ├─ I was parsing a platform header, and I expected the ───┐
└┬──────────────────┘  `provides` section.                                    │
 │                                                                            │
 │  pkg: 77"..c", mm} #                                                       │
 │          ‾‾‾                                                               │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:12:9 ┘

    A platform header must map host symbols to Roc functions in a `provides`
    record.

    For example:
        provides { "roc_main": main }

    I found `..c` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: 77"..c", mm} #                                                       │
 │             ‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_030.md:12:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: 77"..c", mm} #                                                       │
 │              ‾                                                             │
 └─────────────────────────────────────────────────── fuzz_crash_030.md:12:13 ┘

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
 │  pkg: 77"..c", mm} #                                                       │
 │                ‾‾                                                          │
 └─────────────────────────────────────────────────── fuzz_crash_030.md:12:15 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `mm` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: 77"..c", mm} #                                                       │
 │                  ‾                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_030.md:12:17 ┘

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
 │  provides # Cd                                                             │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:13:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `provides` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  [ # pen                                                                   │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:14:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `[` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ar,                                                                       │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:15:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `ar` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ar,                                                                       │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:15:3 ┘

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
 │  ]                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_030.md:16:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,CloseCurly,
OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,Dot,
CloseSquare,
KwPackages,
OpenCurly,
LowerIdent,OpColon,Int,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_provides"))
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
#
# Cd
# pen

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
