# META
~~~ini
description=Record pattern in a `match` branch without `..` is closed: matching a record with an extra field is a type mismatch
type=snippet
~~~
# SOURCE
~~~roc
describe : { x : U64, y : U64, z : U64 } -> U64
describe = |rec| match rec {
    { x, y } => x + y
}
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_match.md:2:18:2:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 18) (end 4 2))
		(headline
			(reflow "The first pattern in this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-underlines
				(display (file "destructure_closed_match.md") (start 2 18) (end 4 2) (annotation dim) (line-text "describe = |rec| match rec {\n    { x, y } => x + y\n}"))
				(underline (start 3 5) (end 3 13) (annotation error)))
			(line-break)
			(reflow "The first pattern is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: U64, y: U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: U64, y: U64, z: U64 }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This pattern doesn't bind the")
			(reflow " ")
			(annotated code "z")
			(reflow " ")
			(reflow "field. Match it explicitly with")
			(reflow " ")
			(annotated code "z: _")
			(reflow ",")
			(reflow " ")
			(reflow "or add")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "to match all the remaining fields."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "describe")
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
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-match
					(e-ident (raw "rec"))
					(branches
						(branch
							(p-record
								(field (name "x") (rest false))
								(field (name "y") (rest false)))
							(e-binop (op "+")
								(e-ident (raw "x"))
								(e-ident (raw "y"))))))))))
~~~
# FORMATTED
~~~roc
describe : { x : U64, y : U64, z : U64 } -> U64
describe = |rec| match rec {
	{ x, y } => x + y
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-runtime-error (tag "erroneous_value_expr")))
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
