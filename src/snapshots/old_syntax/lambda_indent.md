# META
~~~ini
description=lambda_indent
type=expr
~~~
# SOURCE
~~~roc
\x ->
  1
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**lambda_indent.md:1:1:1:3:**
```roc
\x ->
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),LowerIdent(1:2-1:3),OpArrow(1:4-1:6),Newline(1:1-1:1),
Int(2:3-2:4),EndOfFile(2:4-2:4),
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
