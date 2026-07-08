# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# EXPECTED
EXPECTED EXPOSING LIST - fuzz_crash_011.md:1:8:1:9
UNEXPECTED STATEMENT - fuzz_crash_011.md:1:9:1:10
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_011.md:2:1:2:1
# PROBLEMS

┌────────────────────────┐
│ EXPECTED EXPOSING LIST ├─ I was parsing a module or hosted header, and I ───┐
└┬───────────────────────┘  expected an opening `[`.                          │
 │                                                                            │
 │  module P]F                                                                │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_011.md:1:8 ┘

    The names exposed by this module are written in square brackets after the
    header keyword.

    For example:
        module [main, helper]

    I found `P` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module P]F                                                                │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_011.md:1:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_011.md:2:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I reached the end of the file before this construct was complete.

# TOKENS
~~~zig
KwModule,UpperIdent,CloseSquare,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
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
