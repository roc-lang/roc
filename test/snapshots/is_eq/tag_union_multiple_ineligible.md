# META
~~~ini
description=Tag union with multiple tags that have function payloads - shows which tags are ineligible
type=snippet
~~~
# SOURCE
~~~roc
x = Ok("hello")
y = Validate(|n| n > 0)
z = Transform(|s| s)
w = Err("error")
result = if True { x } else if True { y } else if True { z } else { w }
expect result == result
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - tag_union_multiple_ineligible.md:6:8:6:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 6 8) (end 6 24))
		(headline
			(reflow "This expression is doing an equality check on a type that doesn't support equality."))
		(document
			(source-region (file "tag_union_multiple_ineligible.md") (start 6 8) (end 6 24) (annotation error) (line-text "expect result == result"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Err(a), Ok(b), Transform(c -> c), Validate(d -> Bool), ..]")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "    b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    d.is_gt : d, d -> Bool,")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "This tag union does not support equality because these tags have payload types that don't support ")
			(annotated emphasis "is_eq")
			(reflow ":")
			(line-break)
			(line-break)
			(text "    ")
			(annotated emphasis "Transform")
			(text " (")
			(annotated type "a -> a")
			(text ")")
			(line-break)
			(text "        ")
			(reflow "Function equality is not supported.")
			(line-break)
			(text "    ")
			(annotated emphasis "Validate")
			(text " (")
			(annotated type "a -> Bool\n  where [\n    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),\n    a.is_gt : a, a -> Bool,\n  ]")
			(text ")")
			(line-break)
			(text "        ")
			(reflow "Function equality is not supported.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Tag unions only have an ")
			(annotated emphasis "is_eq")
			(reflow " method if all of their payload types have ")
			(annotated emphasis "is_eq")
			(reflow " methods.")
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpGreaterThan,Int,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,OpenCurly,LowerIdent,CloseCurly,
KwExpect,LowerIdent,OpEquals,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-decl
			(p-ident (raw "y"))
			(e-apply
				(e-tag (raw "Validate"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-binop (op ">")
						(e-ident (raw "n"))
						(e-int (raw "0"))))))
		(s-decl
			(p-ident (raw "z"))
			(e-apply
				(e-tag (raw "Transform"))
				(e-lambda
					(args
						(p-ident (raw "s")))
					(e-ident (raw "s")))))
		(s-decl
			(p-ident (raw "w"))
			(e-apply
				(e-tag (raw "Err"))
				(e-string
					(e-string-part (raw "error")))))
		(s-decl
			(p-ident (raw "result"))
			(e-if-then-else
				(e-tag (raw "True"))
				(e-block
					(statements
						(e-ident (raw "x"))))
				(e-if-then-else
					(e-tag (raw "True"))
					(e-block
						(statements
							(e-ident (raw "y"))))
					(e-if-then-else
						(e-tag (raw "True"))
						(e-block
							(statements
								(e-ident (raw "z"))))
						(e-block
							(statements
								(e-ident (raw "w"))))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-ident (raw "result"))))))
~~~
# FORMATTED
~~~roc
x = Ok("hello")

y = Validate(|n| n > 0)

z = Transform(|s| s)

w = Err("error")

result = if True {
	x
} else if True {
	y
} else if True {
	z
} else {
	w
}
expect result == result
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-tag (name "Ok")
			(args
				(e-string
					(e-literal (string "hello"))))))
	(d-let
		(p-assign (ident "y"))
		(e-tag (name "Validate")
			(args
				(e-lambda
					(args
						(p-assign (ident "n")))
					(e-dispatch-call (method "is_gt") (constraint-fn-var 262)
						(receiver
							(e-lookup-local
								(p-assign (ident "n"))))
						(args
							(e-num (value "0"))))))))
	(d-let
		(p-assign (ident "z"))
		(e-tag (name "Transform")
			(args
				(e-lambda
					(args
						(p-assign (ident "s")))
					(e-lookup-local
						(p-assign (ident "s")))))))
	(d-let
		(p-assign (ident "w"))
		(e-tag (name "Err")
			(args
				(e-string
					(e-literal (string "error"))))))
	(d-let
		(p-assign (ident "result"))
		(e-if
			(if-branches
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "x")))))
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "y")))))
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "z"))))))
			(if-else
				(e-block
					(e-lookup-local
						(p-assign (ident "w")))))))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]")))
	(expressions
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(Dec -> Bool), ..]"))))
~~~
