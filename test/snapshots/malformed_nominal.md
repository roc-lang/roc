# META
~~~ini
description=Malformed nominal declaration name recovers with unexpected-statement diagnostics
type=file:malformed_nominal.roc
~~~
# SOURCE
~~~roc
@2 := {}

foo = "one"

bar = "two"
~~~
# EXPECTED
UNEXPECTED STATEMENT - malformed_nominal.md:1:1:1:3
UNEXPECTED STATEMENT - malformed_nominal.md:1:4:1:6
UNEXPECTED STATEMENT - malformed_nominal.md:1:7:1:8
UNEXPECTED STATEMENT - malformed_nominal.md:1:8:1:9
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  @2 := {}                                                                  │
 │  ‾‾                                                                        │
 └────────────────────────────────────────────────── malformed_nominal.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `@2` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  @2 := {}                                                                  │
 │     ‾‾                                                                     │
 └────────────────────────────────────────────────── malformed_nominal.md:1:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:=` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  @2 := {}                                                                  │
 │        ‾                                                                   │
 └────────────────────────────────────────────────── malformed_nominal.md:1:7 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `{` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  @2 := {}                                                                  │
 │         ‾                                                                  │
 └────────────────────────────────────────────────── malformed_nominal.md:1:8 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

# TOKENS
~~~zig
OpaqueName,OpColonEqual,OpenCurly,CloseCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
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
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "one"))))
		(s-decl
			(p-ident (raw "bar"))
			(e-string
				(e-string-part (raw "two"))))))
~~~
# FORMATTED
~~~roc


foo = "one"

bar = "two"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "one"))))
	(d-let
		(p-assign (ident "bar"))
		(e-string
			(e-literal (string "two")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
