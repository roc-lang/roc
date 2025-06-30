# META
~~~ini
description=two_arg_closure
type=expr
~~~
# SOURCE
~~~roc
\a, b -> 42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\a** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**two_arg_closure.md:1:1:1:3:**
```roc
\a, b -> 42
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),LowerIdent(1:2-1:3),Comma(1:3-1:4),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),Int(1:10-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
