# META
~~~ini
description=Polymorphic tuple function with instantiation crash
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap(1, 2)
~~~
# EXPECTED
TOO MANY ARGS - test_tuple_instantiation_crash.md:9:8:9:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Too Many Args")
		(region (start 9 8) (end 9 18))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "swap")
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
			(source-region (file "test_tuple_instantiation_crash.md") (start 9 8) (end 9 18) (annotation error) (line-text "main = swap(1, 2)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "swap")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) -> (b, a)")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "swap")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swap"))
			(e-lambda
				(args
					(p-tuple
						(p-ident (raw "x"))
						(p-ident (raw "y"))))
				(e-tuple
					(e-ident (raw "y"))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "swap"))
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swap"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-assign (ident "x"))
						(p-assign (ident "y")))))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "y")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tuple
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))
				(ty-tuple
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a, b) -> (b, a)"))
		(patt (type "_c")))
	(expressions
		(expr (type "(a, b) -> (b, a)"))
		(expr (type "_c"))))
~~~
