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
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

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
			(e-ident @1.2-1.3 (qaul "") (raw "i")))
		(e-ident @2.1-2.2 (qaul "") (raw "p"))))
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
(e-block @1.1-2.3 (id 78)
	(s-expr @1.2-2.2
		(e-runtime-error (tag "not_implemented")))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "Error"))
~~~
