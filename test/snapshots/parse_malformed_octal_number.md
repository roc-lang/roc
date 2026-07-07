# META
~~~ini
description=Malformed octal number (0o without digits)
type=expr
~~~
# SOURCE
~~~roc
0o
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - parse_malformed_octal_number.md:1:1:1:3
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  0o                                                                        │
 │  ‾‾                                                                        │
 └─────────────────────────────────────── parse_malformed_octal_number.md:1:1 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `0o` here.

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
