# META
~~~ini
description=opt_record_field_pat_assign
type=expr
~~~
# SOURCE
~~~roc
{e?f
4}=f
 e
r
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),NoSpaceOpQuestion(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
Int(2:1-2:2),CloseCurly(2:2-2:3),OpAssign(2:3-2:4),LowerIdent(2:4-2:5),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.3
	(statements
		(e-question-suffix @1.2-1.4
			(e-ident @1.2-1.3 (qaul "") (raw "e")))
		(e-ident @1.4-1.5 (qaul "") (raw "f"))
		(e-int @2.1-2.2 (raw "4"))))
~~~
# FORMATTED
~~~roc
{
	e?
	f
	4
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.3
	(s-expr @1.2-1.5
		(e-runtime-error (tag "not_implemented")))
	(s-expr @1.4-2.2
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-int @2.1-2.2 (value "4")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.3 (type "Num(*)"))
~~~
