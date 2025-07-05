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
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

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
