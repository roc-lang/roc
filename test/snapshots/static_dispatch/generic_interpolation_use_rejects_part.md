# META
~~~ini
description=A generalized interpolation used at Str rejects a non-Str part at that use, leaving other uses intact.
type=snippet
~~~
# SOURCE
~~~roc
greet = |name| "hi ${name}"

ok : Str
ok = greet("there")

bad : Str
bad = greet(42.U64)
~~~
# EXPECTED
TYPE MISMATCH - generic_interpolation_use_rejects_part.md:1:22:1:26
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 22) (end 1 26))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "generic_interpolation_use_rejects_part.md") (start 1 22) (end 1 26) (annotation error) (line-text "greet = |name| \"hi ${name}\""))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "greet"))
			(e-lambda
				(args
					(p-ident (raw "name")))
				(e-string
					(e-string-part (raw "hi "))
					(e-ident (raw "name"))
					(e-string-part (raw "")))))
		(s-type-anno (name "ok")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "ok"))
			(e-apply
				(e-ident (raw "greet"))
				(e-string
					(e-string-part (raw "there")))))
		(s-type-anno (name "bad")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "bad"))
			(e-apply
				(e-ident (raw "greet"))
				(e-typed-int (raw "42") (type "U64"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "greet"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-lookup-local
						(p-assign (ident "name"))))
				(e-interpolation (constraint-fn-var 252) (dispatcher-var 9)
					(first
						(e-literal (string "hi ")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string "")))))))
	(d-let
		(p-assign (ident "ok"))
		(e-call (constraint-fn-var 270)
			(e-lookup-local
				(p-assign (ident "greet")))
			(e-string
				(e-literal (string "there"))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(d-let
		(p-assign (ident "bad"))
		(e-call (constraint-fn-var 286)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-typed-int (value "42") (type "U64")))
		(annotation
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [b.from_interpolation : Str, Iter((a, Str)) -> b]"))
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "a -> b where [b.from_interpolation : Str, Iter((a, Str)) -> b]"))
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
