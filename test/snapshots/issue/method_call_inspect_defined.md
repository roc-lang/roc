# META
~~~ini
description=Method call syntax with .inspect() on string should work now that Str.inspect exists
type=expr
~~~
# SOURCE
~~~roc
{ x = "hello"; x.inspect() }
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - method_call_inspect_defined.md:1:14:1:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 14) (end 1 15))
		(headline
			(reflow "I was parsing an expression, and this token cannot start an expression here."))
		(document
			(reflow "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ";")
			(text " here.")
			(line-break)
			(reflow "This token is malformed, so it cannot be used as ordinary Roc syntax.")
			(line-break)
			(line-break)
			(source-region (file "method_call_inspect_defined.md") (start 1 14) (end 1 15) (annotation error) (line-text "{ x = \"hello\"; x.inspect() }")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,MalformedUnknownToken,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-string
				(e-string-part (raw "hello"))))
		(e-malformed (reason "expr_unexpected_token"))
		(e-method-call (method ".inspect")
			(receiver
				(e-ident (raw "x")))
			(args))))
~~~
# FORMATTED
~~~roc
{
	x = "hello"
		x.inspect()
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-string
			(e-literal (string "hello"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(e-dispatch-call (method "inspect") (constraint-fn-var 219)
		(receiver
			(e-lookup-local
				(p-assign (ident "x"))))
		(args)))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
