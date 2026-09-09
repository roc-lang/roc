# META
~~~ini
description=Type alias with associated items produces error
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_associated.md:1:16:1:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Alias With Associated Items")
		(region (start 1 16) (end 1 17))
		(headline
			(reflow "I was parsing a type alias, but only nominal types can have associated items."))
		(document
			(reflow "Use ")
			(annotated code ":=")
			(reflow " to define a nominal type with associated items, or remove the associated item block from this alias.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := U64 implements [")
			(line-break)
			(indent 1)
			(text "    zero = @Id 0")
			(line-break)
			(indent 1)
			(text "]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "type_alias_with_associated.md") (start 1 16) (end 1 17) (annotation error) (line-text "Foo : [A, B, C].{ x = 5 }")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B"))
					(ty (name "C"))))
			(associated
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
Foo : [A, B, C].{
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.x"))
		(e-num (value "5")))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Dec"))))
~~~
