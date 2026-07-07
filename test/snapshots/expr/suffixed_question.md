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
UNEXPECTED EXPRESSION SYNTAX - suffixed_question.md:1:14:1:15
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  Stdout.line???                                                            │
 │               ‾                                                            │
 └───────────────────────────────────────────────── suffixed_question.md:1:14 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `?` here.

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
