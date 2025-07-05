# META
~~~ini
description=nested_list_comment_in_closure_arg
type=expr
~~~
# SOURCE
~~~roc
\I[[
O#
,i]]->i
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nested_list_comment_in_closure_arg.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\I** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nested_list_comment_in_closure_arg.md:1:1:1:3:**
```roc
\I[[
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpenSquare(1:3-1:4),OpenSquare(1:4-1:5),Newline(1:1-1:1),
UpperIdent(2:1-2:2),Newline(2:3-2:3),
Comma(3:1-3:2),LowerIdent(3:2-3:3),CloseSquare(3:3-3:4),CloseSquare(3:4-3:5),OpArrow(3:5-3:7),LowerIdent(3:7-3:8),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
