# META
~~~ini
description=A local value annotated with an undeclared type is reported, and its value is erroneous rather than typed as an error.
type=snippet
~~~
# SOURCE
~~~roc
f = |_| {
    r : Nope
    r = 7
    "done"
}
~~~
# EXPECTED
UNDECLARED TYPE - local_annotation_undeclared_type.md:2:9:2:13
UNUSED VARIABLE - local_annotation_undeclared_type.md:3:5:3:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 2 9) (end 2 13))
		(headline
			(reflow "The type ")
			(annotated code "Nope")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "local_annotation_undeclared_type.md") (start 2 9) (end 2 13) (annotation error) (line-text "    r : Nope"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 5) (end 3 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "r")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_r")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "local_annotation_undeclared_type.md") (start 3 5) (end 3 6) (annotation error) (line-text "    r = 7")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-anno (name "r")
							(ty (name "Nope")))
						(s-decl
							(p-ident (raw "r"))
							(e-int (raw "7")))
						(e-string
							(e-string-part (raw "done")))))))))
~~~
# FORMATTED
~~~roc
f = |_| {
	r : Nope
	r = 7
	"done"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-runtime-error (tag "erroneous_value_expr"))
				(e-string
					(e-literal (string "done")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")))
	(expressions
		(expr (type "_arg -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))))
~~~
