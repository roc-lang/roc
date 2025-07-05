# META
~~~ini
description=ann_record_pat_with_comment
type=expr
~~~
# SOURCE
~~~roc
{l#
:s}:s
o
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:4-1:4),
OpColon(2:1-2:2),LowerIdent(2:2-2:3),CloseCurly(2:3-2:4),OpColon(2:4-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-2.4
	(field (field "l") (optional false)
		(e-ident @2.2-2.3 (raw "s"))))
~~~
# FORMATTED
~~~roc
{
	l: s
}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-2.4
	(fields
		(field (name "l")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "{ l: Error }"))
~~~
