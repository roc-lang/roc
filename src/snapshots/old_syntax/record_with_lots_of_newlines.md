# META
~~~ini
description=record_with_lots_of_newlines
type=expr
~~~
# SOURCE
~~~roc
{t#
,

}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `t` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:4-1:4),
Comma(2:1-2:2),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-4.2
	(field (field "t") (optional false)))
~~~
# FORMATTED
~~~roc
{
	t


}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-4.2
	(fields
		(field (name "t")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "{ t: Error }"))
~~~
