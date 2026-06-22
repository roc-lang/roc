# META
~~~ini
description=Multiline without comma formatting module
type=snippet
~~~
# SOURCE
~~~roc
	a,
	b
]

a = 'a'
b = 'a'
~~~
# EXPECTED
PARSE ERROR - module.md:1:2:1:3
PARSE ERROR - module.md:1:3:1:4
PARSE ERROR - module.md:2:2:2:3
PARSE ERROR - module.md:3:1:3:2
# PROBLEMS
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   a,                                                                        │
│   ‾                                                                         │
└────────────────────────────────────────────────────────────── module.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   a,                                                                        │
│    ‾                                                                        │
└────────────────────────────────────────────────────────────── module.md:1:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   b                                                                         │
│   ‾                                                                         │
└────────────────────────────────────────────────────────────── module.md:2:2 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  ]                                                                          │
│  ‾                                                                          │
└────────────────────────────────────────────────────────────── module.md:3:1 ┘

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpAssign,SingleQuote,
LowerIdent,OpAssign,SingleQuote,
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
		(s-decl
			(p-ident (raw "a"))
			(e-single-quote (raw "'a'")))
		(s-decl
			(p-ident (raw "b"))
			(e-single-quote (raw "'a'")))))
~~~
# FORMATTED
~~~roc




a = 'a'

b = 'a'
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "97")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
