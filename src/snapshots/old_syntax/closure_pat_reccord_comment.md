# META
~~~ini
description=closure_pat_reccord_comment
type=expr
~~~
# SOURCE
~~~roc
\{i#
,e}->a
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\{** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_pat_reccord_comment.md:1:1:1:3:**
```roc
\{i#
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:5-1:5),
Comma(2:1-2:2),LowerIdent(2:2-2:3),CloseCurly(2:3-2:4),OpArrow(2:4-2:6),LowerIdent(2:6-2:7),EndOfFile(2:7-2:7),
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
