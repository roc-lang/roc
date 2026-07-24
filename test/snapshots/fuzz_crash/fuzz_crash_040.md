# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_040.md:1:20:1:21
UNEXPECTED TYPE SYNTAX - fuzz_crash_040.md:2:3:2:4
UNEXPECTED STATEMENT - fuzz_crash_040.md:2:4:2:5
MALFORMED TYPE - fuzz_crash_040.md:2:3:2:4
DECLARATION HAS NO VALUE - fuzz_crash_040.md:2:1:2:4
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  app[]{f:platform""}{                                                      │
 │                     ‾                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_040.md:1:20 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `{` here.


┌────────────────────────┐
│ UNEXPECTED TYPE SYNTAX ├─ I was parsing a type annotation, and this token ──┐
└┬───────────────────────┘  cannot start a type here.                         │
 │                                                                            │
 │  o:0)                                                                      │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:3 ┘

    Types can be type variables, uppercase type names, function types, tuples,
    records, or tag unions.

    For example:
        List(U64)

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  o:0)                                                                      │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  o:0)                                                                      │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:3 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  o:0)                                                                      │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.

# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,
LowerIdent,OpColon,Int,CloseRound,
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
		(s-type-anno (name "o")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }

o : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "o"))
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
