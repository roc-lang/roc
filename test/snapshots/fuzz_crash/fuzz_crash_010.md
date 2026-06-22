# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNCLOSED STRING - fuzz_crash_010.md:5:5:5:35
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
PARSE ERROR - fuzz_crash_010.md:1:3:1:4
PARSE ERROR - fuzz_crash_010.md:1:4:1:5
PARSE ERROR - fuzz_crash_010.md:2:6:2:7
# PROBLEMS
ASCII CONTROL CHARACTER
ASCII control characters are not allowed in Roc source code.


                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│      "on        (string 'onmo %')))                                         │
│      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
└────────────────────────────────────────────────────── fuzz_crash_010.md:5:5 ┘

                                                                 ┌─────────────┐
┌─ Type applications require parentheses around their type ──────┤ PARSE ERROR │
│  arguments.                                                    └────────────┬┘
│                                                                             │
│  H{o,                                                                       │
│   ‾                                                                         │
└────────────────────────────────────────────────────── fuzz_crash_010.md:1:2 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  H{o,                                                                       │
│    ‾                                                                        │
└────────────────────────────────────────────────────── fuzz_crash_010.md:1:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  H{o,                                                                       │
│     ‾                                                                       │
└────────────────────────────────────────────────────── fuzz_crash_010.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│      ]                                                                     │
│       ‾                                                                     │
└────────────────────────────────────────────────────── fuzz_crash_010.md:2:6 ┘

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
UpperIdent,OpenCurly,LowerIdent,Comma,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "on        (string 'onmo %')))"))))))
~~~
# FORMATTED
~~~roc


foo = 

	"on        (string 'onmo %')))"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
