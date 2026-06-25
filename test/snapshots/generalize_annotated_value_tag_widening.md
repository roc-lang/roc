# META
~~~ini
description=An annotated, non-expansive value generalizes its extension variable, so it is usable at a wider tag union (tier-2 generalization)
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
NIL
# PROBLEMS
NIL
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
		(e-tag (name "Red"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Red"))
				(ty-tag-name (name "Green"))
				(ty-rigid-var (name "#others")))))
	(d-let
		(p-assign (ident "g"))
		(e-lookup-local
			(p-assign (ident "f")))
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
		(patt (type "[Green, Red, ..]"))
		(patt (type "[Blue, Green, Red, ..]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "[Green, Red, ..]"))
		(expr (type "[Blue, Green, Red, ..]"))
		(expr (type "_arg -> {}"))))
~~~
