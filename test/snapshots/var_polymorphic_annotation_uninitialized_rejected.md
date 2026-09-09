# META
~~~ini
description=An uninitialized mutable var whose annotation introduces an unbound type variable is rejected, suggesting `_` to infer the type instead
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    var xs : List(a)
    {}
}
~~~
# EXPECTED
VAR NAME MISSING `$` - var_polymorphic_annotation_uninitialized_rejected.md:4:9:4:11
UNUSED VARIABLE - var_polymorphic_annotation_uninitialized_rejected.md:4:9:4:11
POLYMORPHIC VAR - var_polymorphic_annotation_uninitialized_rejected.md:4:5:4:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 4 9) (end 4 11))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "xs")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$xs")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "var_polymorphic_annotation_uninitialized_rejected.md") (start 4 9) (end 4 11) (annotation warning) (line-text "    var xs : List(a)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 9) (end 4 11))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "xs")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_xs")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "var_polymorphic_annotation_uninitialized_rejected.md") (start 4 9) (end 4 11) (annotation error) (line-text "    var xs : List(a)"))))
	(report
		(severity runtime_error)
		(title "Polymorphic Var")
		(region (start 4 5) (end 4 21))
		(headline
			(reflow "This var is declared with a polymorphic type annotation, but a mutable variable must have a single concrete type."))
		(document
			(source-region (file "var_polymorphic_annotation_uninitialized_rejected.md") (start 4 5) (end 4 21) (annotation error) (line-text "    var xs : List(a)"))
			(line-break)
			(line-break)
			(reflow "Give it a concrete type, or replace the type variable with")
			(reflow " ")
			(annotated code "_")
			(reflow " ")
			(reflow "to let the type be inferred from how the")
			(reflow " ")
			(annotated code "var")
			(reflow " ")
			(reflow "is used."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-anno (name "xs")
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	var xs : List(a)
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var-uninitialized
					(p-var-assign (ident "xs")))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "_arg -> {}"))))
~~~
