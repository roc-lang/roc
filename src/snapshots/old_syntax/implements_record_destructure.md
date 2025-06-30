# META
~~~ini
description=implements_record_destructure
type=expr
~~~
# SOURCE
~~~roc
{implements}=d
I
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **implements}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**implements_record_destructure.md:1:2:1:13:**
```roc
{implements}=d
```
 ^^^^^^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),KwImplements(1:2-1:12),CloseCurly(1:12-1:13),OpAssign(1:13-1:14),LowerIdent(1:14-1:15),Newline(1:1-1:1),
UpperIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.13
	(statements
		(e-malformed @1.2-1.13 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.13
	(e-empty_record @1.1-1.13))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "{}"))
~~~
