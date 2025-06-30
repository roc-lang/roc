# META
~~~ini
description=pattern_in_parens_open fail
type=expr
~~~
# SOURCE
~~~roc
\( a
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pattern_in_parens_open.md:1:1:1:3:**
```roc
\( a
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:4-1:5),EndOfFile(1:5-1:5),
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
