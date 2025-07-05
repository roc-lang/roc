# META
~~~ini
description=record_comment_newline_field
type=expr
~~~
# SOURCE
~~~roc
{#
a}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:3-1:3),
LowerIdent(2:1-2:2),CloseCurly(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.3
	(statements
		(e-ident @2.1-2.2 (raw "a"))))
~~~
# FORMATTED
~~~roc
{
	a
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.3
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "Error"))
~~~
