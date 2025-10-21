# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpFatArrow,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-anno (name "create_user!")
	(ty-fn
		(ty (name "Str"))
		(ty (name "U32"))
		(ty-record
			(anno-record-field (name "name")
				(ty (name "Str")))
			(anno-record-field (name "age")
				(ty (name "U32")))
			(anno-record-field (name "id")
				(ty (name "U64")))
			(anno-record-field (name "active")
				(ty (name "Bool"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "create_user!")
		(ty-fn (effectful true)
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "U32") (builtin))
			(ty-record
				(field (field "name")
					(ty-lookup (name "Str") (builtin)))
				(field (field "age")
					(ty-lookup (name "U32") (builtin)))
				(field (field "id")
					(ty-lookup (name "U64") (builtin)))
				(field (field "active")
					(ty-lookup (name "Bool") (external (module-idx "2") (target-node-idx "1"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
