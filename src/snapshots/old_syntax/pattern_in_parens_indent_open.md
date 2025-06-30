# META
~~~ini
description=pattern_in_parens_indent_open fail
type=expr
~~~
# SOURCE
~~~roc
\(
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pattern_in_parens_indent_open.md:1:1:1:3:**
```roc
\(
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),NoSpaceOpenRound(1:2-1:3),EndOfFile(1:3-1:3),
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
