# META
~~~ini
description=Unmatchable match patterns are reported for the whole branch when every alternative is unmatchable, and per alternative otherwise
type=snippet
~~~
# SOURCE
~~~roc
describe : Try(I64, []) -> I64
describe = |t| match t {
	Ok(_) | Err(_) => 1
	Err(_) | Err(_) => 0
}
~~~
# EXPECTED
UNMATCHABLE PATTERN - match_unmatchable_alternative.md:3:10:3:16
UNMATCHABLE PATTERN - match_unmatchable_alternative.md:4:2:4:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 3 10) (end 3 16))
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
			(reflow "can never match."))
		(document
			(source-region (file "match_unmatchable_alternative.md") (start 3 10) (end 3 16) (annotation warning) (line-text "\tOk(_) | Err(_) => 1"))
			(line-break)
			(reflow "This pattern matches a type that has no possible values (an uninhabited type), so no value can ever match it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 4 2) (end 4 17))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "can never match."))
		(document
			(source-region (file "match_unmatchable_alternative.md") (start 4 2) (end 4 17) (annotation warning) (line-text "\tErr(_) | Err(_) => 0"))
			(line-break)
			(reflow "Each of these patterns matches a type that has no possible values (an uninhabited type), so no value can ever match it."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,CloseSquare,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,Int,
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
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags)))
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
								(p-tag (raw "Ok")
									(p-underscore))
								(p-tag (raw "Err")
									(p-underscore)))
							(e-int (raw "1")))
						(branch
							(p-alternatives
								(p-tag (raw "Err")
									(p-underscore))
								(p-tag (raw "Err")
									(p-underscore)))
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
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Try(I64, []) -> I64")))
	(expressions
		(expr (type "Try(I64, []) -> I64"))))
~~~
