# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| { x: x, y: y }.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "add")
			(ty-fn
				(ty (name "I32"))
				(ty (name "I32"))
				(ty (name "I32"))))
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-field-access
					(e-record
						(field (field "x")
							(e-ident (raw "x")))
						(field (field "y")
							(e-ident (raw "y"))))
					(e-ident (raw "x")))))
		(s-type-anno (name "double")
			(ty-fn
				(ty (name "I32"))
				(ty (name "I32"))))
		(s-decl
			(p-ident (raw "double"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "add"))
					(e-ident (raw "x"))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-apply
				(e-ident (raw "add"))
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-dot-access (field "x")
				(receiver
					(e-record
						(fields
							(field (name "x")
								(e-lookup-local
									(p-assign (ident "x"))))
							(field (name "y")
								(e-lookup-local
									(p-assign (ident "y")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I32") (builtin))
				(ty-lookup (name "I32") (builtin))
				(ty-lookup (name "I32") (builtin)))))
	(d-let
		(p-assign (ident "double"))
		(e-closure
			(captures
				(capture (ident "add")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-call
					(e-lookup-local
						(p-assign (ident "add")))
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I32") (builtin))
				(ty-lookup (name "I32") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-call
			(e-lookup-local
				(p-assign (ident "add")))
			(e-num (value "1"))
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Signed32)), Num(Int(Signed32)) -> Num(Int(Signed32))"))
		(patt (type "Num(Int(Signed32)) -> Num(Int(Signed32))"))
		(patt (type "Num(Int(Signed32))")))
	(expressions
		(expr (type "Num(Int(Signed32)), Num(Int(Signed32)) -> Num(Int(Signed32))"))
		(expr (type "Num(Int(Signed32)) -> Num(Int(Signed32))"))
		(expr (type "Num(Int(Signed32))"))))
~~~
