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
VAR NAME MISSING `$` - fmt_var_in_record_field.md:1:10:1:11
UNUSED VARIABLE - fmt_var_in_record_field.md:1:10:1:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "c")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$c")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "fmt_var_in_record_field.md") (start 1 10) (end 1 11) (annotation warning) (line-text "f=||{var c:[]}"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "c")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_c")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fmt_var_in_record_field.md") (start 1 10) (end 1 11) (annotation error) (line-text "f=||{var c:[]}")))))
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
					(p-var-assign (ident "c")))
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
