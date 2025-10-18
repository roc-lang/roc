# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "platform/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "platform/main.roc"))))))
	(statements
		(s-type-anno (name "helper")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "helper"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-binop (op "*")
					(e-ident (raw "n"))
					(e-int (raw "2")))))
		(s-type-anno (name "main")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore)
					(p-underscore))
				(e-apply
					(e-ident (raw "helper"))
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "helper"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "n")))
				(e-num (value "2"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin))))))
	(d-let
		(p-assign (ident "main"))
		(e-closure
			(captures
				(capture (ident "helper")))
			(e-lambda
				(args
					(p-underscore)
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "helper")))
					(e-num (value "5")))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt (type "Num(Int(Signed64)), Num(Int(Signed64)) -> Num(Int(Signed64))")))
	(expressions
		(expr (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr (type "Num(Int(Signed64)), Num(Int(Signed64)) -> Num(Int(Signed64))"))))
~~~
