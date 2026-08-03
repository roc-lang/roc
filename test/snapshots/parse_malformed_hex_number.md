# META
~~~ini
description=Malformed hex number (0x without digits)
type=expr
~~~
# SOURCE
~~~roc
0x
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - parse_malformed_hex_number.md:1:1:1:3
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  0x                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────── parse_malformed_hex_number.md:1:1 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `0x` here.

# TOKENS
~~~zig
MalformedNumberNoDigits,
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
