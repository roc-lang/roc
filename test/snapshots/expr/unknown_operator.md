# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - unknown_operator.md:1:4:1:5
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  1 ++ 2                                                                    │
 │     ‾                                                                      │
 └─────────────────────────────────────────────────── unknown_operator.md:1:4 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `+` here.

# TOKENS
~~~zig
Int,OpPlus,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-int (raw "1"))
	(e-malformed (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
1 + 
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
