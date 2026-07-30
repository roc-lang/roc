# META
~~~ini
description=An empty mod with multiline exposes and comments
type=snippet
~~~
# SOURCE
~~~roc
	[ # Comment After exposes open
		something, # Comment after exposed item
		SomeType, # Comment after final exposed item
	]
~~~
# EXPECTED
UNEXPECTED STATEMENT - mod_multiline_with_comments.md:1:2:1:3
UNEXPECTED STATEMENT - mod_multiline_with_comments.md:2:3:2:12
UNEXPECTED STATEMENT - mod_multiline_with_comments.md:2:12:2:13
TYPE APPLICATION NEEDS PARENTHESES - mod_multiline_with_comments.md:3:11:3:12
UNEXPECTED STATEMENT - mod_multiline_with_comments.md:4:2:4:3
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  [ # Comment After exposes open                                            │
 │  ‾                                                                         │
 └───────────────────────────────────── mod_multiline_with_comments.md:1:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `[` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  something, # Comment after exposed item                                   │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └───────────────────────────────────── mod_multiline_with_comments.md:2:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `something` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  something, # Comment after exposed item                                   │
 │           ‾                                                                │
 └──────────────────────────────────── mod_multiline_with_comments.md:2:12 ┘

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
 │  SomeType, # Comment after final exposed item                              │
 │          ‾                                                                 │
 └──────────────────────────────────── mod_multiline_with_comments.md:3:11 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ]                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────── mod_multiline_with_comments.md:4:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
OpenSquare,
LowerIdent,Comma,
UpperIdent,Comma,
CloseSquare,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# Comment After exposes open
# Comment after final exposed item
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
