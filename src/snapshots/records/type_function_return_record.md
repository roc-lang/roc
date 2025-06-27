# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:13),OpColon(1:14-1:15),UpperIdent(1:16-1:19),Comma(1:19-1:20),UpperIdent(1:21-1:24),OpFatArrow(1:25-1:27),OpenCurly(1:28-1:29),LowerIdent(1:30-1:34),OpColon(1:35-1:36),UpperIdent(1:37-1:40),Comma(1:40-1:41),LowerIdent(1:42-1:45),OpColon(1:46-1:47),UpperIdent(1:48-1:51),Comma(1:51-1:52),LowerIdent(1:53-1:55),OpColon(1:56-1:57),UpperIdent(1:58-1:61),Comma(1:61-1:62),LowerIdent(1:63-1:69),OpColon(1:70-1:71),UpperIdent(1:72-1:76),CloseCurly(1:77-1:78),EndOfFile(1:78-1:78),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-78 (name "create_user!")
	(ty-fn @1-16-1-78
		(ty (name "Str"))
		(ty (name "U32"))
		(ty-record @1-28-1-78
			(anno-record-field @1-30-1-41 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-42-1-52 (name "age")
				(ty (name "U32")))
			(anno-record-field @1-53-1-62 (name "id")
				(ty (name "U64")))
			(anno-record-field @1-63-1-78 (name "active")
				(ty (name "Bool"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @1-1-1-78 (name "create_user!") (id 85)
		(ty-fn @1-16-1-78 (effectful true)
			(ty @1-16-1-19 (name "Str"))
			(ty @1-21-1-24 (name "U32"))
			(ty-record @1-28-1-78
				(field (field "name")
					(ty @1-37-1-40 (name "Str")))
				(field (field "age")
					(ty @1-48-1-51 (name "U32")))
				(field (field "id")
					(ty @1-58-1-61 (name "U64")))
				(field (field "active")
					(ty @1-72-1-76 (name "Bool")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
