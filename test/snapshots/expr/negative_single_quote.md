# META
~~~ini
description=Negative single quote char literal
type=expr
~~~
# SOURCE
~~~roc
-'i'
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - negative_single_quote.md:1:1:1:2
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  -'i'                                                                      │
 │  ‾                                                                         │
 └────────────────────────────────────────────── negative_single_quote.md:1:1 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `-` here.

# TOKENS
~~~zig
OpBinaryMinus,SingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

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
