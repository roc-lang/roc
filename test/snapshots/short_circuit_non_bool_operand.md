# META
~~~ini
description=A non-Bool operand of `and` or `or` is reported as a bool operation and retires the operator, leaving the operand's producer intact
type=snippet
~~~
# SOURCE
~~~roc
describe : U64 -> [Verbose(U64)]
describe = |n| Verbose(n)

both = |x| Bool.True and describe(x)

either = |_| Quiet or Bool.False

again = |n| describe(n)
~~~
# EXPECTED
TYPE MISMATCH - short_circuit_non_bool_operand.md:4:26:4:37
TYPE MISMATCH - short_circuit_non_bool_operand.md:6:14:6:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 26) (end 4 37))
		(headline
			(reflow "I'm having trouble with this bool operation."))
		(document
			(source-region (file "short_circuit_non_bool_operand.md") (start 4 26) (end 4 37) (annotation error) (line-text "both = |x| Bool.True and describe(x)"))
			(line-break)
			(reflow "Both sides of")
			(reflow " ")
			(annotated code "and")
			(reflow " ")
			(reflow "must be")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "values, but the")
			(reflow " ")
			(reflow "right")
			(reflow " ")
			(reflow "side is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Verbose(U64)]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "Roc does not have \"truthiness\". You must convert values to bools yourself.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 14) (end 6 19))
		(headline
			(reflow "I'm having trouble with this bool operation."))
		(document
			(source-region (file "short_circuit_non_bool_operand.md") (start 6 14) (end 6 19) (annotation error) (line-text "either = |_| Quiet or Bool.False"))
			(line-break)
			(reflow "Both sides of")
			(reflow " ")
			(annotated code "or")
			(reflow " ")
			(reflow "must be")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "values, but the")
			(reflow " ")
			(reflow "left")
			(reflow " ")
			(reflow "side is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Quiet]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "Roc does not have \"truthiness\". You must convert values to bools yourself."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,OpAnd,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,OpOr,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "describe")
			(ty-fn
				(ty (name "U64"))
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Verbose"))
							(ty (name "U64")))))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-tag (raw "Verbose"))
					(e-ident (raw "n")))))
		(s-decl
			(p-ident (raw "both"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "and")
					(e-tag (raw "Bool.True"))
					(e-apply
						(e-ident (raw "describe"))
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "either"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "or")
					(e-tag (raw "Quiet"))
					(e-tag (raw "Bool.False")))))
		(s-decl
			(p-ident (raw "again"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "describe"))
					(e-ident (raw "n")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-tag (name "Verbose")
				(args
					(e-lookup-local
						(p-assign (ident "n"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-tag-union
					(ty-tag-name (name "Verbose")
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "both"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(d-let
		(p-assign (ident "either"))
		(e-lambda
			(args
				(p-underscore))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(d-let
		(p-assign (ident "again"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-call (constraint-fn-var 299)
				(e-lookup-local
					(p-assign (ident "describe")))
				(e-lookup-local
					(p-assign (ident "n")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> [Verbose(U64)]"))
		(patt (type "U64 -> Bool"))
		(patt (type "_arg -> Bool"))
		(patt (type "U64 -> [Verbose(U64)]")))
	(expressions
		(expr (type "U64 -> [Verbose(U64)]"))
		(expr (type "U64 -> Bool"))
		(expr (type "_arg -> Bool"))
		(expr (type "U64 -> [Verbose(U64)]"))))
~~~
