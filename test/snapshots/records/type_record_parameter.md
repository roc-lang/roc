# META
~~~ini
description=Function type annotation with record parameter
type=statement
~~~
# SOURCE
~~~roc
process_things : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,Comma,OpenRound,LowerIdent,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-anno (name "process_things")
	(ty-fn
		(ty-record
			(anno-record-field (name "name")
				(ty (name "Str")))
			(anno-record-field (name "age")
				(ty (name "U32")))
			(anno-record-field (name "thing")
				(ty-var (raw "a"))))
		(ty-fn
			(ty-var (raw "a"))
			(ty (name "Str")))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
process_things : { name : Str, age : U32, thing : a }, (a -> Str) -> Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "process_things")
		(ty-fn (effectful false)
			(ty-record
				(field (field "name")
					(ty-lookup (name "Str") (builtin)))
				(field (field "age")
					(ty-lookup (name "U32") (builtin)))
				(field (field "thing")
					(ty-rigid-var (name "a"))))
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Str") (builtin))))
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
