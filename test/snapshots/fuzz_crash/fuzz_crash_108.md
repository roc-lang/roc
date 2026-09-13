# META
~~~ini
description=canonicalize crash: missing checked pattern for exhaustiveness site
type=file
~~~
# SOURCE
~~~roc
main!=|0|||"".P
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - fuzz_crash_108.md:1:8:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Non Exhaustive Destructure")
		(region (start 1 8) (end 1 9))
		(headline
			(reflow "This destructuring pattern doesn't cover all possible cases."))
		(document
			(source-region (file "fuzz_crash_108.md") (start 1 8) (end 1 9) (annotation error) (line-text "main!=|0|||\"\".P"))
			(line-break)
			(reflow "The value being destructured has type:")
			(line-break)
			(text "        ")
			(annotated type "a\n  where [\n    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),\n    a.is_eq : a, a -> Bool,\n  ]")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "_")
			(annotation-end)
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Int,OpBar,OpBar,OpBar,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,
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
					(p-int (raw "0")))
				(e-lambda
					(args)
					(e-typed-string (type "P")
						(e-string-part (raw ""))))))))
~~~
# FORMATTED
~~~roc
main! = |0| || "".P
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "a -> (({}) -> Error) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "a -> (({}) -> Error) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]"))))
~~~
