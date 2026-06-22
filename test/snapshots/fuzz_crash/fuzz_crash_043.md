# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0}0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_043.md:1:20:1:21
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_043.md:2:3:2:4
PARSE ERROR - fuzz_crash_043.md:2:4:2:5
PARSE ERROR - fuzz_crash_043.md:2:5:2:6
MALFORMED TYPE - fuzz_crash_043.md:2:3:2:4
DECLARATION HAS NO VALUE - fuzz_crash_043.md:2:1:2:4
# PROBLEMS
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  app[]{f:platform""}{                                                       │
│                     ‾                                                       │
└───────────────────────────────────────────────────── fuzz_crash_043.md:1:20 ┘

    This is an unexpected parsing error. Please check your syntax.
                                         ┌─────────────────────────────────────┐
┌─ The token 0 is not expected in a ─────┤ UNEXPECTED TOKEN IN TYPE ANNOTATION │
│  type annotation.                      └────────────────────────────────────┬┘
│                                                                             │
│  o:0}0                                                                      │
│    ‾                                                                        │
└────────────────────────────────────────────────────── fuzz_crash_043.md:2:3 ┘

    Type annotations should contain types like Str, Num a, or List U64.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  o:0}0                                                                      │
│     ‾                                                                       │
└────────────────────────────────────────────────────── fuzz_crash_043.md:2:4 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  o:0}0                                                                      │
│      ‾                                                                      │
└────────────────────────────────────────────────────── fuzz_crash_043.md:2:5 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                              ┌────────────────┐
┌─ This type annotation is malformed or contains invalid ─────┤ MALFORMED TYPE │
│  syntax.                                                    └───────────────┬┘
│                                                                             │
│  o:0}0                                                                      │
│    ‾                                                                        │
└────────────────────────────────────────────────────── fuzz_crash_043.md:2:3 ┘

                                                    ┌──────────────────────────┐
┌─ This declaration has a type annotation but no ───┤ DECLARATION HAS NO VALUE │
│  implementation.                                  └─────────────────────────┬┘
│                                                                             │
│  o:0}0                                                                      │
│  ‾‾‾                                                                        │
└────────────────────────────────────────────────────── fuzz_crash_043.md:2:1 ┘

    Add a value body here, or put hosted functions in a platform type module so they are published through the host boundary.
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,
LowerIdent,OpColon,Int,CloseCurly,Int,
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
		(s-malformed (tag "statement_unexpected_token"))
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
