# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_024.md:2:1:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:2:8:2:9
PARSE ERROR - fuzz_crash_024.md:5:1:5:4
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:2:8:2:9
DUPLICATE DEFINITION - fuzz_crash_024.md:5:5:5:6
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: var_only_allowed_in_a_body ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  var t= ]                                                                  │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_024.md:2:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token ] is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  var t= ]                                                                  │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_024.md:2:8 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: var_only_allowed_in_a_body ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  var t= 0                                                                  │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_024.md:5:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  var t= ]                                                                  │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_024.md:2:8 ┘

    This might be a syntax error, an unsupported language feature, or a typo.


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `t` is being redeclared here. ─────────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  var t= 0                                                                  │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_024.md:5:5 ┘

    In this scope, `t` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    2 │  var t= ]                                                             │
      │      ‾                                                                │
      └──────────────────────────────────────────────── fuzz_crash_024.md:2:5 ┘

# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,CloseSquare,
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
# el
t = 

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "t"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Error"))
		(expr (type "Dec"))))
~~~
