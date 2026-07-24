# META
~~~ini
description=Issue #10094: Invalid formatting for package exposing syntax
type=file
~~~
# SOURCE
~~~roc
dapkage[e,E.a.*]{}
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:1:1:8
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:8:1:9
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:9:1:10
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:10:1:11
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_094.md:1:12:1:14
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:14:1:16
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:16:1:17
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:17:1:18
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:18:1:19
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │  ‾‾‾‾‾‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_094.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `dapkage` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_094.md:1:8 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `[` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_094.md:1:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `e` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │           ‾                                                                │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │             ‾‾                                                             │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:12 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `.a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │               ‾‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:14 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.*` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │                 ‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │                  ‾                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:17 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `{` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  dapkage[e,E.a.*]{}                                                        │
 │                   ‾                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_094.md:1:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
LowerIdent,OpenSquare,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,DotStar,CloseSquare,OpenCurly,CloseCurly,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
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
