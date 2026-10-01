# META
~~~ini
description=Issue 11731: constant closure condition in expression checking
type=expr
~~~
# SOURCE
~~~roc
|hay| {
    contains = |n| Str.contains("999", n)
    if contains("99") { hay } else { "free" }
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - issue_11731_constant_closure_condition.md:3:8:3:22
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 3 8) (end 3 22))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "issue_11731_constant_closure_condition.md") (start 3 8) (end 3 22) (annotation warning) (line-text "    if contains(\"99\") { hay } else { \"free\" }")))))
~~~
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseRound,
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
						(e-string
							(e-string-part (raw "999")))
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
	contains = |n| Str.contains("999", n)
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
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-call (constraint-fn-var 243)
					(e-lookup-external
						(builtin))
					(e-string
						(e-literal (string "999")))
					(e-lookup-local
						(p-assign (ident "n"))))))
		(e-if
			(if-branches
				(if-branch
					(e-call (constraint-fn-var 252)
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
(expr (type "a -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
~~~
