# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# EXPECTED
UNCLOSED SINGLE QUOTE - fuzz_crash_031.md:4:10:4:11
PARSE ERROR - fuzz_crash_031.md:1:1:1:5
PARSE ERROR - fuzz_crash_031.md:1:6:1:7
PARSE ERROR - fuzz_crash_031.md:1:7:1:8
PARSE ERROR - fuzz_crash_031.md:4:1:4:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_031.md:4:10:4:11
UNRECOGNIZED SYNTAX - fuzz_crash_031.md:4:10:4:11
# PROBLEMS
                                                       ┌───────────────────────┐
┌─ This single-quoted literal is missing a closing ────┤ UNCLOSED SINGLE QUOTE │
│  quote.                                              └──────────────────────┬┘
│                                                                             │
│  vavar t= '                                                                 │
│           ‾                                                                 │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:4:10

                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  mule []                                                                    │
│  ‾‾‾‾                                                                       │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:1:1

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  mule []                                                                    │
│       ‾                                                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:1:6

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  mule []                                                                    │
│        ‾                                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:1:7

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  vavar t= '                                                                 │
│  ‾‾‾‾‾                                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:4:1

    This is an unexpected parsing error. Please check your syntax.
                                              ┌────────────────────────────────┐
┌─ The token ' is not expected in an ─────────┤ UNEXPECTED TOKEN IN EXPRESSION │
│  expression.                                └───────────────────────────────┬┘
│                                                                             │
│  vavar t= '                                                                 │
│           ‾                                                                 │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:4:10

    Expressions can be identifiers, literals, function calls, or operators.
                                                         ┌─────────────────────┐
┌─ I don't recognize this syntax. ───────────────────────┤ UNRECOGNIZED SYNTAX │
│                                                        └────────────────────┬┘
│                                                                             │
│  vavar t= '                                                                 │
│           ‾                                                                 │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_031.md:4:10

    This might be a syntax error, an unsupported language feature, or a typo.
# TOKENS
~~~zig
LowerIdent,OpenSquare,CloseSquare,
LowerIdent,LowerIdent,OpAssign,MalformedSingleQuote,
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
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc


# el
t = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
