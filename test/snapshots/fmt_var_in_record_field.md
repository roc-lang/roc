# META
~~~ini
description=Formatter preserves var keyword in record field annotations
type=snippet
~~~
# SOURCE
~~~roc
f=||{var c:[]}
~~~
# EXPECTED
UNUSED VARIABLE - fmt_var_in_record_field.md:1:6:1:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 6) (end 1 14))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "c")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_c")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fmt_var_in_record_field.md") (start 1 6) (end 1 14) (annotation error) (line-text "f=||{var c:[]}")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwVar,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
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
				(args)
				(e-block
					(statements
						(s-type-anno (name "c")
							(ty-tag-union
								(tags)))))))))
~~~
# FORMATTED
~~~roc
f = || {
	var c : []
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(s-var-uninitialized
					(p-assign (ident "c")))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "({}) -> {}"))))
~~~
