# META
~~~ini
description=Formatter stability for open tag unions with blank lines before ..
type=snippet
~~~
# SOURCE
~~~roc
r : [
	a,

	..,
]
~~~
# EXPECTED
MALFORMED TYPE - open_tag_union_stability.md:2:2:2:3
DECLARATION HAS NO VALUE - open_tag_union_stability.md:1:1:5:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 2 2) (end 2 3))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "open_tag_union_stability.md") (start 2 2) (end 2 3) (annotation error) (line-text "\ta,"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 5 2))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "open_tag_union_stability.md") (start 1 1) (end 5 2) (annotation error) (line-text "r : [\n\ta,\n\n\t..,\n]"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,
LowerIdent,Comma,
DoubleDot,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "r")
			(ty-tag-union
				(tags
					(ty-var (raw "a")))
				..))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tag-union
				(ty-malformed)
				(ty-rigid-var (name "#others"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
