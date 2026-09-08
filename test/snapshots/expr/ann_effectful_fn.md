# META
~~~ini
description=Annotated effectful function
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Try Bool LaunchNukeErr
    launchTheNukes = |{}| ...

    launchTheNukes({})
}
~~~
# EXPECTED
DECLARATION HAS NO VALUE - ann_effectful_fn.md:2:5:2:31
TYPE MISMATCH - ann_effectful_fn.md:2:32:2:36
TYPE MISMATCH - ann_effectful_fn.md:2:37:2:50
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 5) (end 2 31))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "ann_effectful_fn.md") (start 2 5) (end 2 31) (annotation error) (line-text "    launchTheNukes : {} => Try Bool LaunchNukeErr"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 32) (end 2 36))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "ann_effectful_fn.md") (start 2 32) (end 2 36) (annotation error) (line-text "    launchTheNukes : {} => Try Bool LaunchNukeErr"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Bool, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 37) (end 2 50))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "ann_effectful_fn.md") (start 2 37) (end 2 50) (annotation error) (line-text "    launchTheNukes : {} => Try Bool LaunchNukeErr"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[LaunchNukeErr, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow "."))))
~~~
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpFatArrow,UpperIdent,UpperIdent,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,TripleDot,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "launchTheNukes")
			(ty-fn
				(ty-record)
				(ty (name "Try"))))
		(e-tag (raw "Bool"))
		(e-tag (raw "LaunchNukeErr"))
		(s-decl
			(p-ident (raw "launchTheNukes"))
			(e-lambda
				(args
					(p-record))
				(e-ellipsis)))
		(e-apply
			(e-ident (raw "launchTheNukes"))
			(e-record))))
~~~
# FORMATTED
~~~roc
{
	launchTheNukes : {} => Try
	Bool
	LaunchNukeErr
	launchTheNukes = |{}| ...

	launchTheNukes({})
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "launchTheNukes"))
		(e-anno-only))
	(s-expr
		(e-tag (name "Bool")))
	(s-expr
		(e-tag (name "LaunchNukeErr")))
	(s-let
		(p-assign (ident "launchTheNukes"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-not-implemented)))
	(e-call (constraint-fn-var 255)
		(e-lookup-local
			(p-assign (ident "launchTheNukes")))
		(e-empty_record)))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
