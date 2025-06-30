# META
~~~ini
description=space_after_opt_field_pat
type=expr
~~~
# SOURCE
~~~roc
{p?
m}:J
O
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

**UNDEFINED VARIABLE**
Nothing is named `m` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),NoSpaceOpQuestion(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseCurly(2:2-2:3),OpColon(2:3-2:4),UpperIdent(2:4-2:5),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.3
	(statements
		(e-question-suffix @1.2-1.4
			(e-ident @1.2-1.3 (qaul "") (raw "p")))
		(e-ident @2.1-2.2 (qaul "") (raw "m"))))
~~~
# FORMATTED
~~~roc
{
	p?
	m
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.3 (id 78)
	(s-expr @1.2-2.2
		(e-runtime-error (tag "not_implemented")))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "Error"))
~~~
