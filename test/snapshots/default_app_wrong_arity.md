# META
~~~ini
description=Error - main! with wrong number of parameters
type=file
mod_validation_diagnostics=true
~~~
# SOURCE
~~~roc
main! = |arg1, arg2| {
    arg1
}
~~~
# EXPECTED
UNUSED VARIABLE - default_app_wrong_arity.md:1:16:1:20
`MAIN!` SHOULD TAKE 1 ARGUMENT - default_app_wrong_arity.md:1:1:3:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 16) (end 1 20))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "arg2")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_arg2")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "default_app_wrong_arity.md") (start 1 16) (end 1 20) (annotation error) (line-text "main! = |arg1, arg2| {"))))
	(report
		(severity runtime_error)
		(title "`main!` Should Take 1 Argument")
		(region (start 1 1) (end 3 2))
		(headline
			(annotated code "main!")
			(reflow " is defined but has the wrong number of arguments. ")
			(annotated code "main!")
			(reflow " should take 1 argument."))
		(document
			(text "Found ")
			(annotated code "2")
			(reflow " arguments.")
			(line-break)
			(line-break)
			(reflow "Change it to:")
			(line-break)
			(annotated code "main! = |arg| { ... }")
			(line-break)
			(source-region (file "default_app_wrong_arity.md") (start 1 1) (end 3 2) (annotation error) (line-text "main! = |arg1, arg2| {\n    arg1\n}")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "arg1"))
					(p-ident (raw "arg2")))
				(e-block
					(statements
						(e-ident (raw "arg1"))))))))
~~~
# FORMATTED
~~~roc
main! = |arg1, arg2| {
	arg1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "arg1"))
				(p-assign (ident "arg2")))
			(e-block
				(e-lookup-local
					(p-assign (ident "arg1")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, _arg -> a")))
	(expressions
		(expr (type "a, _arg -> a"))))
~~~
