# META
~~~ini
description=empty_record_update
type=expr
~~~
# SOURCE
~~~roc
{e&}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**empty_record_update.md:1:3:1:5:**
```roc
{e&}
```
  ^^


**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),OpAmpersand(1:3-1:4),CloseCurly(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.5
	(statements
		(e-ident @1.2-1.3 (qaul "") (raw "e"))
		(e-malformed @1.3-1.5 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	e
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.5 (id 77)
	(s-expr @1.2-1.4
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-empty_record @1.1-1.5))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "{}"))
~~~
