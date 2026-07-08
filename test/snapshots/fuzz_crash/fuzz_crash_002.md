# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - fuzz_crash_002.md:1:6:1:7
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:7:1:9
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:9:1:11
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:11:1:13
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:13:1:15
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:15:1:17
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:17:1:19
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:19:1:21
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:21:1:23
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:23:1:24
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:24:1:25
MALFORMED TYPE - fuzz_crash_002.md:1:6:1:7
DECLARATION HAS NO VALUE - fuzz_crash_002.md:1:1:1:7
# PROBLEMS

┌────────────────────────┐
│ UNEXPECTED TYPE SYNTAX ├─ I was parsing a type annotation, and this token ──┐
└┬───────────────────────┘  cannot start a type here.                         │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │       ‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_002.md:1:6 ┘

    Types can be type variables, uppercase type names, function types, tuples,
    records, or tag unions.

    For example:
        List(U64)

    I found `;` here.
    This token is malformed, so it cannot be used as ordinary Roc syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │        ‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_002.md:1:7 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │          ‾‾                                                                │
 └───────────────────────────────────────────────────── fuzz_crash_002.md:1:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │            ‾‾                                                              │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │              ‾‾                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:13 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                ‾‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:15 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                  ‾‾                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:17 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                    ‾‾                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:19 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `::` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                      ‾‾                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:21 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `le` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                        ‾                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:23 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `[` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │                         ‾                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_002.md:1:24 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `%` here.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │       ‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_002.md:1:6 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  modu:;::::::::::::::le[%                                                  │
 │  ‾‾‾‾‾‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_002.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,MalformedUnknownToken,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,LowerIdent,OpenSquare,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "modu")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
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
modu : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "modu"))
		(e-anno-only)
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
