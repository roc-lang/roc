# META
~~~ini
description=Repro for https://github.com/roc-lang/roc/issues/12152 - a redundant alternative inside a single match branch is reported at that alternative, not at the match keyword as "the first branch"
type=snippet
~~~
# SOURCE
~~~roc
describe : [Foo(I64), Bar] -> I64
describe = |t| match t {
	Foo(x) | Foo(x) => x
	_ => 0
}
~~~
# EXPECTED
REDUNDANT PATTERN - match_redundant_alternative_issue_12152.md:3:11:3:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Redundant Pattern")
		(region (start 3 11) (end 3 17))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "is redundant."))
		(document
			(source-region (file "match_redundant_alternative_issue_12152.md") (start 3 11) (end 3 17) (annotation warning) (line-text "\tFoo(x) | Foo(x) => x"))
			(line-break)
			(reflow "This pattern can never match because earlier patterns already cover all the values it would match."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
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
						(ty (name "Bar"))))
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
							(p-alternatives
								(p-tag (raw "Foo")
									(p-ident (raw "x")))
								(p-tag (raw "Foo")
									(p-ident (raw "x"))))
							(e-ident (raw "x")))
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
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "x")))))
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
					(ty-tag-name (name "Bar")))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Bar, Foo(I64)] -> I64")))
	(expressions
		(expr (type "[Bar, Foo(I64)] -> I64"))))
~~~
