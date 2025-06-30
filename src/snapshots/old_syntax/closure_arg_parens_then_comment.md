# META
~~~ini
description=closure_arg_parens_then_comment
type=expr
~~~
# SOURCE
~~~roc
\(8)->T#
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_arg_parens_then_comment.md:1:1:1:3:**
```roc
\(8)->T#
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),CloseRound(1:4-1:5),OpArrow(1:5-1:7),UpperIdent(1:7-1:8),EndOfFile(1:9-1:9),
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
