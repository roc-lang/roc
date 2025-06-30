# META
~~~ini
description=empty_record_newline_assign
type=expr
~~~
# SOURCE
~~~roc

{} 
={}
I
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
{** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**empty_record_newline_assign.md:1:1:2:2:**
```roc

{} 
```


# TOKENS
~~~zig
Newline(1:1-1:1),
OpenCurly(2:1-2:2),CloseCurly(2:2-2:3),Newline(1:1-1:1),
OpAssign(3:1-3:2),OpenCurly(3:2-3:3),CloseCurly(3:3-3:4),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-2.2 (reason "expr_unexpected_token"))
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
