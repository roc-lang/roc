# META
~~~ini
description=Nominal tuple construction with Type.(a, b) syntax
type=snippet
~~~
# SOURCE
~~~roc
Pair := (U64, Str)

p : Pair
p = Pair.(1, "x")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Pair")
				(args))
			(ty-tuple
				(ty (name "U64"))
				(ty (name "Str"))))
		(s-type-anno (name "p")
			(ty (name "Pair")))
		(s-decl
			(p-ident (raw "p"))
			(e-nominal-apply
				(mapper (e-tag (raw "Pair")))
				(e-int (raw "1"))
				(e-string
					(e-string-part (raw "x")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "p"))
		(e-nominal (nominal "Pair")
			(e-tuple
				(elems
					(e-num (value "1"))
					(e-string
						(e-literal (string "x"))))))
		(annotation
			(ty-lookup (name "Pair") (local))))
	(s-nominal-decl
		(ty-header (name "Pair"))
		(ty-tuple
			(ty-lookup (name "U64") (builtin))
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Pair")))
	(type_decls
		(nominal (type "Pair")
			(ty-header (name "Pair"))))
	(expressions
		(expr (type "Pair"))))
~~~
