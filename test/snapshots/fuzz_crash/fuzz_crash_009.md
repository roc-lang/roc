# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_009.md:6:5:6:12
PARSE ERROR - fuzz_crash_009.md:1:2:1:3
PARSE ERROR - fuzz_crash_009.md:1:3:1:4
PARSE ERROR - fuzz_crash_009.md:1:4:1:5
PARSE ERROR - fuzz_crash_009.md:1:5:1:6
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
# PROBLEMS
                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│      "onmo %                                                                │
│      ‾‾‾‾‾‾‾                                                                │
└────────────────────────────────────────────────────── fuzz_crash_009.md:6:5 ┘

                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   f{o,                                                                      │
│   ‾                                                                         │
└────────────────────────────────────────────────────── fuzz_crash_009.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   f{o,                                                                      │
│    ‾                                                                        │
└────────────────────────────────────────────────────── fuzz_crash_009.md:1:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   f{o,                                                                      │
│     ‾                                                                       │
└────────────────────────────────────────────────────── fuzz_crash_009.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   f{o,                                                                      │
│      ‾                                                                      │
└────────────────────────────────────────────────────── fuzz_crash_009.md:1:5 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│       ]                                                                     │
│       ‾                                                                     │
└────────────────────────────────────────────────────── fuzz_crash_009.md:2:6 ┘

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
LowerIdent,OpenCurly,LowerIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "onmo %"))))))
~~~
# FORMATTED
~~~roc



foo = 

	"onmo %"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "onmo %")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
