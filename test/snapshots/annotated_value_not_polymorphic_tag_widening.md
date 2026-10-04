# META
~~~ini
description=An explicit `..` on a value annotation is rejected, since a value cannot quantify a row; a value alias of it still reports its own redundant `..`
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

f : [Red, Green, ..]
f = Red

g : [Red, Green, Blue, ..]
g = f

main! = |_| {}
~~~
# EXPECTED
VALUE IS NOT POLYMORPHIC - annotated_value_not_polymorphic_tag_widening.md:3:1:3:21
REDUNDANT OPEN TAG UNION - annotated_value_not_polymorphic_tag_widening.md:6:24:6:26
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Value Is Not Polymorphic")
		(region (start 3 1) (end 3 21))
		(headline
			(reflow "The type annotation on")
			(reflow " ")
			(annotated code "f")
			(reflow " ")
			(reflow "says it can be used at many types, but")
			(reflow " ")
			(annotated code "f")
			(reflow " ")
			(reflow "is not a function, so it can only have one type."))
		(document
			(source-region (file "annotated_value_not_polymorphic_tag_widening.md") (start 3 1) (end 3 21) (annotation error) (line-text "f : [Red, Green, ..]"))
			(line-break)
			(line-break)
			(reflow "If you want me to infer its type, remove each")
			(reflow " ")
			(annotated code "..")
			(reflow ", or write a concrete type.")
			(line-break)
			(line-break)
			(reflow "If you want to use it at many types, make it a function that takes")
			(reflow " ")
			(annotated code "{}")
			(reflow ":")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "f : {} -> [Red, Green, ..]")
			(line-break)
			(indent 1)
			(text "f = |{}| Red")
			(annotation-end)
			(line-break)
			(reflow "Then call it as")
			(reflow " ")
			(annotated code "f({})")
			(reflow " ")
			(reflow "wherever you use it.")))
	(report
		(severity warning)
		(title "Redundant Open Tag Union")
		(region (start 6 24) (end 6 26))
		(headline
			(reflow "This tag union has an explicit `..`, but it is already implicitly open."))
		(document
			(source-region (file "annotated_value_not_polymorphic_tag_widening.md") (start 6 24) (end 6 26) (annotation warning) (line-text "g : [Red, Green, Blue, ..]"))
			(line-break)
			(line-break)
			(reflow "Tag unions in output positions, like the return type of a function, are automatically open. Remove the")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "or bind it to a named type variable like")
			(reflow " ")
			(annotated code "..others")
			(reflow " ")
			(reflow "if you want to refer to the extension elsewhere."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,DoubleDot,CloseSquare,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,DoubleDot,CloseSquare,
LowerIdent,OpAssign,LowerIdent,
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
		(s-type-anno (name "f")
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green")))
				..))
		(s-decl
			(p-ident (raw "f"))
			(e-tag (raw "Red")))
		(s-type-anno (name "g")
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue")))
				..))
		(s-decl
			(p-ident (raw "g"))
			(e-ident (raw "f")))
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
		(p-assign (ident "f"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Red"))
				(ty-tag-name (name "Green"))
				(ty-rigid-var (name "#others")))))
	(d-let
		(p-assign (ident "g"))
		(e-runtime-error (tag "erroneous_value_use"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Red"))
				(ty-tag-name (name "Green"))
				(ty-tag-name (name "Blue"))
				(ty-rigid-var (name "#others")))))
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
		(patt (type "[Blue, Green, Red]"))
		(patt (type "[Blue, Green, Red]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "[Blue, Green, Red]"))
		(expr (type "[Blue, Green, Red]"))
		(expr (type "_arg -> {}"))))
~~~
