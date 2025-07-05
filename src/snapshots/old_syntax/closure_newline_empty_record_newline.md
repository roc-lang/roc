# META
~~~ini
description=closure_newline_empty_record_newline
type=expr
~~~
# SOURCE
~~~roc
\L->
 {
}
Θ
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - closure_newline_empty_record_newline.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_newline_empty_record_newline.md:1:1:1:3:**
```roc
\L->
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpArrow(1:3-1:5),Newline(1:1-1:1),
OpenCurly(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),Newline(1:1-1:1),
MalformedUnicodeIdent(4:1-4:3),EndOfFile(4:3-4:3),
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
