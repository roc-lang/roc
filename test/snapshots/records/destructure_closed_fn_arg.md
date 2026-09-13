# META
~~~ini
description=Record destructure in a function parameter without `..` is closed: the pattern must cover every annotated field
type=snippet
~~~
# SOURCE
~~~roc
get_sum : { x : U64, y : U64, z : U64 } -> U64
get_sum = |{ x, y }| x + y
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_fn_arg.md:2:12:2:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 12) (end 2 20))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "destructure_closed_fn_arg.md") (start 2 12) (end 2 20) (annotation error) (line-text "get_sum = |{ x, y }| x + y"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: U64, y: U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: U64, y: U64, z: U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "z"))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "get_sum")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "get_sum"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_sum"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64")))
	(expressions
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))))
~~~
