# META
~~~ini
description=Module dispatch in where clause
type=snippet
~~~
# SOURCE
~~~roc
deserialize : List(U8) -> Result(a, [DecodeErr])
	where [a.decode : List(U8) -> Result(a, [DecodeErr])]
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "deserialize")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "U8")))
				(ty-apply
					(ty (name "Result"))
					(ty-var (raw "a"))
					(ty-tag-union
						(tags
							(ty (name "DecodeErr"))))))
			(where
				(method (module-of "a") (name "decode")
					(args
						(ty-apply
							(ty (name "List"))
							(ty (name "U8"))))
					(ty-apply
						(ty (name "Result"))
						(ty-var (raw "a"))
						(ty-tag-union
							(tags
								(ty (name "DecodeErr"))))))))
		(s-decl
			(p-ident (raw "deserialize"))
			(e-lambda
				(args
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "deserialize"))
		(e-lambda
			(args
				(p-underscore))
			(e-not-implemented))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))
				(ty-apply (name "Result") (external-module "Builtin")
					(ty-rigid-var (name "a"))
					(ty-tag-union
						(ty-tag-name (name "DecodeErr")))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "decode")
					(args
						(ty-apply (name "List") (builtin)
							(ty-lookup (name "U8") (builtin))))
					(ty-apply (name "Result") (external-module "Builtin")
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-tag-union
							(ty-tag-name (name "DecodeErr")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr]) where [List(Num(Int(Unsigned8))).decode : List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr])]")))
	(expressions
		(expr (type "List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr]) where [List(Num(Int(Unsigned8))).decode : List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr])]"))))
~~~
