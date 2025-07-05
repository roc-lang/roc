# META
~~~ini
description=record_update_apply_closure_comments
type=expr
~~~
# SOURCE
~~~roc
{#
h&}\#
 i->0
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_update_apply_closure_comments.md:2:2:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update_apply_closure_comments.md:2:2:2:4:**
```roc
h&}\#
```
 ^^


**UNDEFINED VARIABLE**
Nothing is named `h` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:3-1:3),
LowerIdent(2:1-2:2),OpAmpersand(2:2-2:3),CloseCurly(2:3-2:4),OpBackslash(2:4-2:5),Newline(2:6-2:6),
LowerIdent(3:2-3:3),OpArrow(3:3-3:5),Int(3:5-3:6),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.4
	(statements
		(e-ident @2.1-2.2 (raw "h"))
		(e-malformed @2.2-2.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	h
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.4
	(s-expr @2.1-2.3
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-empty_record @1.1-2.4))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "{}"))
~~~
