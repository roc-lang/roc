# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/11312 - a crash message that is not a Str is reported as a type mismatch whose actual type is the message's own type
type=snippet
~~~
# SOURCE
~~~roc
poly = || {
	crash YYYYY
	"x"
}

rDsult = poly() == poly()
~~~
# EXPECTED
TYPE MISMATCH - issue_11312_crash_non_str_message.md:2:8:2:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 8) (end 2 13))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "issue_11312_crash_non_str_message.md") (start 2 8) (end 2 13) (annotation error) (line-text "\tcrash YYYYY"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[YYYYY]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwCrash,UpperIdent,
StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,LowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "poly"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-tag (raw "YYYYY")))
						(e-string
							(e-string-part (raw "x")))))))
		(s-decl
			(p-ident (raw "rDsult"))
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "poly")))
				(e-apply
					(e-ident (raw "poly")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "poly"))
		(e-lambda
			(args)
			(e-block
				(s-expr
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-string
					(e-literal (string "x"))))))
	(d-let
		(p-assign (ident "rDsult"))
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 235)
					(e-lookup-local
						(p-assign (ident "poly")))))
			(rhs
				(e-call (constraint-fn-var 240)
					(e-lookup-local
						(p-assign (ident "poly"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "Bool")))
	(expressions
		(expr (type "({}) -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "Bool"))))
~~~
