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
PARSE ERROR - fuzz_crash_040.md:1:20:1:21
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_040.md:2:3:2:4
PARSE ERROR - fuzz_crash_040.md:2:4:2:5
MALFORMED TYPE - fuzz_crash_040.md:2:3:2:4
DECLARATION HAS NO VALUE - fuzz_crash_040.md:2:1:2:4
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  app[]{f:platform""}{                                                      │
 │                     ‾                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_040.md:1:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 0 is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  o:0)                                                                      │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:3 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  o:0)                                                                      │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_040.md:2:4 ┘

    This is an unexpected parsing error. Please check your syntax.


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

    Add a value body here, or put hosted functions in a platform type module so
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
