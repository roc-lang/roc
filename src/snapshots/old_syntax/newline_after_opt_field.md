# META
~~~ini
description=newline_after_opt_field
type=expr
~~~
# SOURCE
~~~roc
{i?
p}
~~~
# EXPECTED
not_implemented - newline_after_opt_field.md:1:1:1:1
UNDEFINED VARIABLE - newline_after_opt_field.md:2:1:2:2
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),NoSpaceOpQuestion(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseCurly(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.3
	(statements
		(e-question-suffix @1.2-1.4
			(e-ident @1.2-1.3 (raw "i")))
		(e-ident @2.1-2.2 (raw "p"))))
~~~
# FORMATTED
~~~roc
{
	i?
	p
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.3
	(s-expr @1.2-2.2
		(e-runtime-error (tag "not_implemented")))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "Error"))
~~~
