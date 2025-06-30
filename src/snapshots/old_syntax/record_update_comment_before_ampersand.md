# META
~~~ini
description=record_update_comment_before_ampersand
type=expr
~~~
# SOURCE
~~~roc
{i#
&}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update_comment_before_ampersand.md:2:1:2:3:**
```roc
&}
```
^^


**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:4-1:4),
OpAmpersand(2:1-2:2),CloseCurly(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.3
	(statements
		(e-ident @1.2-1.3 (qaul "") (raw "i"))
		(e-malformed @2.1-2.3 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	i
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.3 (id 77)
	(s-expr @1.2-2.2
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-empty_record @1.1-2.3))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "{}"))
~~~
