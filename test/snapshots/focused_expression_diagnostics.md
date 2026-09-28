# META
~~~ini
description=Issue 11775: match and function diagnostics highlight their introducers and incompatible patterns
type=snippet
~~~
# SOURCE
~~~roc
unused_match = |value| {
    match value {
        A => Ok({})
        B => Ok({})
    }
    {}
}

incompatible_pattern : [A, B] -> U8
incompatible_pattern = |value| match value {
    A(_) => 1
    B => 2
}

missing_case : [A, B] -> U8
missing_case = |value| match value {
    A => 1
}

wrong_function : U8
wrong_function = |value| {
    value
}

wrong_zero_arg_function : U8
wrong_zero_arg_function = || {
    1
}
~~~
# EXPECTED
TYPE MISMATCH - focused_expression_diagnostics.md:2:5:2:10
TYPE MISMATCH - focused_expression_diagnostics.md:10:32:10:32
TYPE MISMATCH - focused_expression_diagnostics.md:21:18:21:25
TYPE MISMATCH - focused_expression_diagnostics.md:26:27:26:29
NON EXHAUSTIVE MATCH - focused_expression_diagnostics.md:16:24:16:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 5) (end 2 10))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "focused_expression_diagnostics.md") (start 2 5) (end 2 10) (annotation error) (line-text "    match value {"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Ok({})]")
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
		(region (start 11 5) (end 11 9))
		(headline
			(reflow "The first pattern in this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-underlines
				(display (file "focused_expression_diagnostics.md") (start 10 32) (end 13 2) (annotation dim) (line-text "incompatible_pattern = |value| match value {\n    A(_) => 1\n    B => 2\n}"))
				(underline (start 11 5) (end 11 9) (annotation error)))
			(line-break)
			(reflow "The first pattern is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A(_a)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the value being matched on has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, B]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 21 18) (end 21 25))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "focused_expression_diagnostics.md") (start 21 18) (end 21 25) (annotation error) (line-text "wrong_function = |value| {"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> a")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 26 27) (end 26 29))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "focused_expression_diagnostics.md") (start 26 27) (end 26 29) (annotation error) (line-text "wrong_zero_arg_function = || {"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "({}) -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Non Exhaustive Match")
		(region (start 16 24) (end 16 29))
		(headline
			(reflow "This match expression doesn't cover all possible cases."))
		(document
			(source-region (file "focused_expression_diagnostics.md") (start 16 24) (end 16 29) (annotation error) (line-text "missing_case = |value| match value {"))
			(line-break)
			(reflow "The value being matched on has type:")
			(line-break)
			(text "        ")
			(annotated type "[A, B]")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "B")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Hint: Add branches to handle these cases, or use")
			(reflow " ")
			(annotated keyword "_")
			(reflow " ")
			(reflow "to match anything."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
OpenCurly,CloseCurly,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "unused_match"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "value"))
							(branches
								(branch
									(p-tag (raw "A"))
									(e-apply
										(e-tag (raw "Ok"))
										(e-record)))
								(branch
									(p-tag (raw "B"))
									(e-apply
										(e-tag (raw "Ok"))
										(e-record)))))
						(e-record)))))
		(s-type-anno (name "incompatible_pattern")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "A"))
						(ty (name "B"))))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "incompatible_pattern"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-match
					(e-ident (raw "value"))
					(branches
						(branch
							(p-tag (raw "A")
								(p-underscore))
							(e-int (raw "1")))
						(branch
							(p-tag (raw "B"))
							(e-int (raw "2")))))))
		(s-type-anno (name "missing_case")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "A"))
						(ty (name "B"))))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "missing_case"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-match
					(e-ident (raw "value"))
					(branches
						(branch
							(p-tag (raw "A"))
							(e-int (raw "1")))))))
		(s-type-anno (name "wrong_function")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "wrong_function"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(e-ident (raw "value"))))))
		(s-type-anno (name "wrong_zero_arg_function")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "wrong_zero_arg_function"))
			(e-lambda
				(args)
				(e-block
					(statements
						(e-int (raw "1"))))))))
~~~
# FORMATTED
~~~roc
unused_match = |value| {
	match value {
		A => Ok({})
		B => Ok({})
	}
	{}
}

incompatible_pattern : [A, B] -> U8
incompatible_pattern = |value| match value {
	A(_) => 1
	B => 2
}

missing_case : [A, B] -> U8
missing_case = |value| match value {
	A => 1
}

wrong_function : U8
wrong_function = |value| {
	value
}

wrong_zero_arg_function : U8
wrong_zero_arg_function = || {
	1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unused_match"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-block
				(s-expr
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-empty_record))))
	(d-let
		(p-assign (ident "incompatible_pattern"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B")))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "missing_case"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "value"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B")))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "wrong_function"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "wrong_zero_arg_function"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "U8") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[A, B] -> {}"))
		(patt (type "[A, B] -> U8"))
		(patt (type "[A, B] -> U8"))
		(patt (type "U8"))
		(patt (type "U8")))
	(expressions
		(expr (type "[A, B] -> {}"))
		(expr (type "[A, B] -> U8"))
		(expr (type "[A, B] -> U8"))
		(expr (type "U8"))
		(expr (type "U8"))))
~~~
