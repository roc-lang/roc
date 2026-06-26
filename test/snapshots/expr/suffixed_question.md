# META
~~~ini
description=suffixed_question
type=expr
~~~
# SOURCE
~~~roc
Stdout.line???
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - suffixed_question.md:1:14:1:15
# PROBLEMS

┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token ? is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  Stdout.line???                                                            │
 │               ‾                                                            │
 └───────────────────────────────────────────────── suffixed_question.md:1:14 ┘

    Expressions can be identifiers, literals, function calls, or operators.

# TOKENS
~~~zig
UpperIdent,NoSpaceDotLowerIdent,OpDoubleQuestion,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "??")
	(e-ident (raw "Stdout.line"))
	(e-malformed (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
Stdout.line ?? 
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
