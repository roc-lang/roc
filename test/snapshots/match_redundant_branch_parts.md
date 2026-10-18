# META
~~~ini
description=Redundant match patterns are reported for the whole branch when every alternative is redundant, and per alternative otherwise
type=snippet
~~~
# SOURCE
~~~roc
describe : [Foo(I64), Bar(I64), Baz] -> I64
describe = |t| match t {
	Foo(x) => x
	Foo(y) | Bar(y) => y
	Bar(z) | Foo(z) => z
	Foo(w) => w
	_ => 0
}
~~~
# EXPECTED
REDUNDANT PATTERN - match_redundant_branch_parts.md:4:2:4:8
REDUNDANT PATTERN - match_redundant_branch_parts.md:5:2:5:17
REDUNDANT PATTERN - match_redundant_branch_parts.md:6:2:6:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Redundant Pattern")
		(region (start 4 2) (end 4 8))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern in the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "is redundant."))
		(document
			(source-region (file "match_redundant_branch_parts.md") (start 4 2) (end 4 8) (annotation warning) (line-text "\tFoo(y) | Bar(y) => y"))
			(line-break)
			(reflow "This pattern can never match because earlier patterns already cover all the values it would match.")))
	(report
		(severity warning)
		(title "Redundant Pattern")
		(region (start 5 2) (end 5 17))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "is redundant."))
		(document
			(source-region (file "match_redundant_branch_parts.md") (start 5 2) (end 5 17) (annotation warning) (line-text "\tBar(z) | Foo(z) => z"))
			(line-break)
			(reflow "These patterns can never match because earlier patterns already cover all the values they would match.")))
	(report
		(severity warning)
		(title "Redundant Pattern")
		(region (start 6 2) (end 6 8))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "fourth")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "is redundant."))
		(document
			(source-region (file "match_redundant_branch_parts.md") (start 6 2) (end 6 8) (annotation warning) (line-text "\tFoo(w) => w"))
			(line-break)
			(reflow "This pattern can never match because earlier patterns already cover all the values it would match."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
Underscore,OpFatArrow,Int,
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
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Foo"))
							(ty (name "I64")))
						(ty-apply
							(ty (name "Bar"))
							(ty (name "I64")))
						(ty (name "Baz"))))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "t")))
				(e-match
					(e-ident (raw "t"))
					(branches
						(branch
							(p-tag (raw "Foo")
								(p-ident (raw "x")))
							(e-ident (raw "x")))
						(branch
							(p-alternatives
								(p-tag (raw "Foo")
									(p-ident (raw "y")))
								(p-tag (raw "Bar")
									(p-ident (raw "y"))))
							(e-ident (raw "y")))
						(branch
							(p-alternatives
								(p-tag (raw "Bar")
									(p-ident (raw "z")))
								(p-tag (raw "Foo")
									(p-ident (raw "z"))))
							(e-ident (raw "z")))
						(branch
							(p-tag (raw "Foo")
								(p-ident (raw "w")))
							(e-ident (raw "w")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
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
				(p-assign (ident "t")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "t"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "x")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "y")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "z")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "w")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Foo")
						(ty-lookup (name "I64") (builtin)))
					(ty-tag-name (name "Bar")
						(ty-lookup (name "I64") (builtin)))
					(ty-tag-name (name "Baz")))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Bar(I64), Baz, Foo(I64)] -> I64")))
	(expressions
		(expr (type "[Bar(I64), Baz, Foo(I64)] -> I64"))))
~~~
