# META
~~~ini
description=Return type mismatch with instantiated function
type=expr
~~~
# SOURCE
~~~roc
{
    identity : a -> a
    identity = |x| x

    needs_string : ((Str -> Str) -> Str)
    needs_string = |f| f(["hello"])

    needs_string(identity)
}
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_return_crash.md:6:24:6:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 24) (end 6 36))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "test_instantiated_return_crash.md") (start 6 24) (end 6 36) (annotation dim) (line-text "    needs_string = |f| f([\"hello\"])"))
				(underline (start 6 26) (end 6 35) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(b) where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "f")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,OpenRound,NoSpaceOpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "identity")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-anno (name "needs_string")
			(ty-fn
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "needs_string"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-list
						(e-string
							(e-string-part (raw "hello")))))))
		(e-apply
			(e-ident (raw "needs_string"))
			(e-ident (raw "identity")))))
~~~
# FORMATTED
~~~roc
{
	identity : a -> a
	identity = |x| x

	needs_string : ((Str -> Str) -> Str)
	needs_string = |f| f(["hello"])

	needs_string(identity)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(s-let
		(p-assign (ident "needs_string"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call
				(e-lookup-local
					(p-assign (ident "f")))
				(e-list
					(elems
						(e-string
							(e-literal (string "hello"))))))))
	(e-call (constraint-fn-var 262)
		(e-lookup-local
			(p-assign (ident "needs_string")))
		(e-lookup-local
			(p-assign (ident "identity")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
