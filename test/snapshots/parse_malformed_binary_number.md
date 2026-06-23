# META
~~~ini
description=Malformed binary number (0b without digits)
type=expr
~~~
# SOURCE
~~~roc
0b
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_binary_number.md:1:1:1:3
# PROBLEMS

┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token 0b is not expected in an ───────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  0b                                                                        │
 │  ‾‾                                                                        │
 └────────────────────────────────────── parse_malformed_binary_number.md:1:1 ┘

    Expressions can be identifiers, literals, function calls, or operators.

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
