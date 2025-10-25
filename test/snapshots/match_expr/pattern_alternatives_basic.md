# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=snippet
~~~
# SOURCE
~~~roc
Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpBar,UpperIdent,OpBar,UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpBar,UpperIdent,OpBar,UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue"))
					(ty (name "Yellow"))
					(ty (name "Orange"))
					(ty (name "Purple")))))
		(s-type-anno (name "kind")
			(ty-fn
				(ty (name "Color"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "kind"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-match
					(e-ident (raw "color"))
					(branches
						(branch
							(p-alternatives
								(p-tag (raw "Red"))
								(p-tag (raw "Green"))
								(p-tag (raw "Blue")))
							(e-string
								(e-string-part (raw "primary"))))
						(branch
							(p-alternatives
								(p-tag (raw "Yellow"))
								(p-tag (raw "Orange"))
								(p-tag (raw "Purple")))
							(e-string
								(e-string-part (raw "secondary"))))))))))
~~~
# FORMATTED
~~~roc
Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
	Red | Green | Blue => "primary"
	Yellow | Orange | Purple => "secondary"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "kind"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "primary")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "secondary")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Color") (local))
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue"))
			(ty-tag-name (name "Yellow"))
			(ty-tag-name (name "Orange"))
			(ty-tag-name (name "Purple")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Color -> Str")))
	(type_decls
		(alias (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color -> Str"))))
~~~
