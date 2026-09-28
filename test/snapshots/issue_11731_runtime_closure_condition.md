# META
~~~ini
description=Issue 11731: runtime closure condition in expression checking
type=expr
~~~
# SOURCE
~~~roc
|hay| {
    contains = |n| Str.contains(hay, n)
    if contains("99") { hay } else { "free" }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
KwIf,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpenCurly,LowerIdent,CloseCurly,KwElse,OpenCurly,StringStart,StringPart,StringEnd,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "hay")))
	(e-block
		(statements
			(s-decl
				(p-ident (raw "contains"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-apply
						(e-ident (raw "Str.contains"))
						(e-ident (raw "hay"))
						(e-ident (raw "n")))))
			(e-if-then-else
				(e-apply
					(e-ident (raw "contains"))
					(e-string
						(e-string-part (raw "99"))))
				(e-block
					(statements
						(e-ident (raw "hay"))))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "free")))))))))
~~~
# FORMATTED
~~~roc
|hay| {
	contains = |n| Str.contains(hay, n)
	if contains("99") {
		hay
	} else {
		"free"
	}
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "hay")))
	(e-block
		(s-let
			(p-assign (ident "contains"))
			(e-closure
				(captures
					(capture (ident "hay")))
				(e-lambda
					(args
						(p-assign (ident "n")))
					(e-call (constraint-fn-var 232)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "hay")))
						(e-lookup-local
							(p-assign (ident "n")))))))
		(e-if
			(if-branches
				(if-branch
					(e-call (constraint-fn-var 241)
						(e-lookup-local
							(p-assign (ident "contains")))
						(e-string
							(e-literal (string "99"))))
					(e-block
						(e-lookup-local
							(p-assign (ident "hay"))))))
			(if-else
				(e-block
					(e-string
						(e-literal (string "free"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str -> Str"))
~~~
