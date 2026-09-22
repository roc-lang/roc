# META
~~~ini
description=A type variable name starting with `$` gets a warning suggesting the name without `$`
type=snippet
~~~
# SOURCE
~~~roc
first : List($elem) -> Try($elem, [Empty])
first = |list| match list {
    [elem, ..] => Ok(elem)
    [] => Err(Empty)
}
~~~
# EXPECTED
TYPE VARIABLE STARTING WITH DOLLAR - type_var_starting_with_dollar.md:1:14:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Type Variable Starting With Dollar")
		(region (start 1 14) (end 1 19))
		(headline
			(reflow "The type variable ")
			(annotated code "$elem")
			(reflow " starts with ")
			(annotated code "$")
			(reflow "."))
		(document
			(source-region (file "type_var_starting_with_dollar.md") (start 1 14) (end 1 19) (annotation error) (line-text "first : List($elem) -> Try($elem, [Empty])"))
			(line-break)
			(reflow "The ")
			(annotated code "$")
			(reflow " prefix is only for variables declared with ")
			(annotated keyword "var")
			(reflow ", and type variables can never be reassigned. Rename it to ")
			(annotated code "elem")
			(reflow " instead."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
OpenSquare,CloseSquare,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "first")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "$elem")))
				(ty-apply
					(ty (name "Try"))
					(ty-var (raw "$elem"))
					(ty-tag-union
						(tags
							(ty (name "Empty")))))))
		(s-decl
			(p-ident (raw "first"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-match
					(e-ident (raw "list"))
					(branches
						(branch
							(p-list
								(p-ident (raw "elem"))
								(p-list-rest))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "elem"))))
						(branch
							(p-list)
							(e-apply
								(e-tag (raw "Err"))
								(e-tag (raw "Empty"))))))))))
~~~
# FORMATTED
~~~roc
first : List($elem) -> Try($elem, [Empty])
first = |list| match list {
	[elem, ..] => Ok(elem)
	[] => Err(Empty)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "first"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "list"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns
											(p-assign (ident "elem")))
										(rest-at (index 1)))))
							(value
								(e-tag (name "Ok")
									(args
										(e-lookup-local
											(p-assign (ident "elem")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-tag (name "Err")
									(args
										(e-tag (name "Empty"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "$elem")))
				(ty-apply (name "Try") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "$elem")))
					(ty-tag-union
						(ty-tag-name (name "Empty"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List($elem) -> Try($elem, [Empty])")))
	(expressions
		(expr (type "List($elem) -> Try($elem, [Empty])"))))
~~~
