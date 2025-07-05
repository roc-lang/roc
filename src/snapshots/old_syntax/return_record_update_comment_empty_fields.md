# META
~~~ini
description=return_record_update_comment_empty_fields
type=expr
~~~
# SOURCE
~~~roc
return
 {#
g&}e
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_record_update_comment_empty_fields.md:1:1:1:1:**
```roc
return
```



# TOKENS
~~~zig
KwReturn(1:1-1:7),Newline(1:1-1:1),
OpenCurly(2:2-2:3),Newline(2:4-2:4),
LowerIdent(3:1-3:2),OpAmpersand(3:2-3:3),CloseCurly(3:3-3:4),LowerIdent(3:4-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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
