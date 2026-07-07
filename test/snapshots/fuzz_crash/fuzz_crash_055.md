# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
module(a).h:s
~~~
# EXPECTED
EXPECTED EXPOSING LIST - fuzz_crash_055.md:1:7:1:8
UNEXPECTED STATEMENT - fuzz_crash_055.md:1:8:1:9
UNEXPECTED STATEMENT - fuzz_crash_055.md:1:9:1:10
UNEXPECTED STATEMENT - fuzz_crash_055.md:1:10:1:12
UNEXPECTED STATEMENT - fuzz_crash_055.md:1:12:1:13
UNEXPECTED STATEMENT - fuzz_crash_055.md:1:13:1:14
# PROBLEMS

┌────────────────────────┐
│ EXPECTED EXPOSING LIST ├─ I was parsing a module or hosted header, and I ───┐
└┬───────────────────────┘  expected an opening `[`.                          │
 │                                                                            │
 │  module(a).h:s                                                             │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_055.md:1:7 ┘

    The names exposed by this module are written in square brackets after the
    header keyword.

    For example:
        module [main, helper]

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).h:s                                                             │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_055.md:1:8 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).h:s                                                             │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_055.md:1:9 ┘

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
 │  module(a).h:s                                                             │
 │           ‾‾                                                               │
 └──────────────────────────────────────────────────── fuzz_crash_055.md:1:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.h` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).h:s                                                             │
 │             ‾                                                              │
 └──────────────────────────────────────────────────── fuzz_crash_055.md:1:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).h:s                                                             │
 │              ‾                                                             │
 └──────────────────────────────────────────────────── fuzz_crash_055.md:1:13 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `s` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.

# TOKENS
~~~zig
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
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
