# META
~~~ini
description=Polymorphic function instantiation with arity mismatch
type=expr
~~~
# SOURCE
~~~roc
{
    identity : (a, b) -> (a, b)
    identity = |pair| pair

    identity(1, 2)
}
~~~
# EXPECTED
TOO MANY ARGS - test_instantiation_arity_mismatch.md:5:5:5:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Too Many Args")
		(region (start 5 5) (end 5 19))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "identity")
			(reflow " function expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument")
			(reflow ",")
			(reflow " ")
			(reflow "but it got")
			(reflow " ")
			(reflow "2")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "test_instantiation_arity_mismatch.md") (start 5 5) (end 5 19) (annotation error) (line-text "    identity(1, 2)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "identity")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) -> (a, b)")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "identity")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-ident (raw "pair"))))
		(e-apply
			(e-ident (raw "identity"))
			(e-int (raw "1"))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	identity : (a, b) -> (a, b)
	identity = |pair| pair

	identity(1, 2)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-lookup-local
				(p-assign (ident "pair")))))
	(e-call (constraint-fn-var 244)
		(e-lookup-local
			(p-assign (ident "identity")))
		(e-num (value "1"))
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "_c"))
~~~
