# META
~~~ini
description=A `_` hole in a top-level value annotation filled only by a numeric literal is generalized with the value, so the value is rejected as polymorphic and the report points at the hole
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

e : Try(_, [Boom])
e = Ok(1)

main! = |_| {}
~~~
# EXPECTED
POLYMORPHIC VALUE - polymorphic_value_annotation_hole.md:4:1:4:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 4 1) (end 4 2))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "polymorphic_value_annotation_hole.md") (start 4 1) (end 4 2) (annotation error) (line-text "e = Ok(1)"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "Try(a, [Boom]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(line-break)
			(reflow "The polymorphic part was inferred for this")
			(reflow " ")
			(annotated code "_")
			(reflow " ")
			(reflow "in its annotation:")
			(line-break)
			(source-region (file "polymorphic_value_annotation_hole.md") (start 3 5) (end 3 19) (annotation error) (line-text "e : Try(_, [Boom])"))
			(line-break)
			(line-break)
			(reflow "It was inferred as:")
			(line-break)
			(annotated code-block "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Write the concrete type you want in place of")
			(reflow " ")
			(annotated code "_")
			(reflow "."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
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
		(s-type-anno (name "e")
			(ty-apply
				(ty (name "Try"))
				(_)
				(ty-tag-union
					(tags
						(ty (name "Boom"))))))
		(s-decl
			(p-ident (raw "e"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-int (raw "1"))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "e"))
		(e-tag (name "Ok")
			(args
				(e-num (value "1"))))
		(annotation
			(ty-apply (name "Try") (builtin)
				(ty-underscore)
				(ty-tag-union
					(ty-tag-name (name "Boom"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Try(a, [Boom]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "Try(a, [Boom]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> {}"))))
~~~
